;;; dida.el --- Dida365 同步的插件 -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2025.06
;; Version: 2.3.0

;; URL: https://github.com/TomoeMami/dida

;; Package-Requires: ((emacs "27.1") (plz "0.9"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 滴答清单（Dida365）的底层 API 库。
;;
;; 本文件只包含 API 层（dida-api-* / dida-auth-*）：
;;    - 统一的 HTTP 请求原语：认证、401 刷新重试、429/5xx 退避、严格 JSON 解析；
;;    - 纯数据的资源函数：项目、任务、已完成窗口、过滤、搜索、批量、移动、删除；
;;    - 错误统一为 `dida-api-error'，带 :kind / :status，可判断是否可重试。
;;
;; 设计约定：API 层不碰 Org、不做同步决策；Org 工作流见 dida-workflow.el
;; （同仓库、随包分发，只用本库的 dida-api-* 与认证函数）。

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defgroup dida nil
  "滴答清单"
  :group 'local)

;;; ---------------------------------------------------------------------------
;;; 1. 用户配置
;;; ---------------------------------------------------------------------------

(defcustom dida-client-id ""
  "注册 dida365 API 时系统生成的 client-id。"
  :type 'string
  :group 'dida)

(defcustom dida-client-secret ""
  "注册 dida365 API 时系统生成的 client-secret。"
  :type 'string
  :group 'dida)

(defcustom dida-auth-scopes "tasks:write tasks:read"
  "API 的权限，默认是读写。"
  :type 'string
  :group 'dida)

(defcustom dida-global-tag nil
  "dida 同步下来 task 的全局 tag，直接插入到 `::' 之间，多个 tag 用 `tag1:tag2' 形式。"
  :type '(choice (const nil) string)
  :group 'dida)

(defcustom dida-token-file (expand-file-name "didatoken" user-emacs-directory)
  "存储 dida token 的文件路径。"
  :type 'file
  :group 'dida)

(defcustom dida-sync-file ""
  "dida 清单同步到的一个文件路径，留空待用户自行设置。"
  :type 'file
  :group 'dida)

(defcustom dida-redirect-uri "http://localhost:1145"
  "注册 dida365 API 时填入的 OAuth redirect url。"
  :type 'string
  :group 'dida)

(defcustom dida-oauth-port 1145
  "本地 OAuth 回调服务器端口，必须与 `dida-redirect-uri' 一致。"
  :type 'integer
  :group 'dida)

;; API 层配置
(defcustom dida-api-base "https://api.dida365.com"
  "Dida365 API 基地址。国际版 TickTick 可设为 `https://api.ticktick.com'。"
  :type 'string
  :group 'dida)

(defcustom dida-oauth-base "https://dida365.com"
  "Dida365 OAuth 基地址。国际版 TickTick 可设为 `https://ticktick.com'。"
  :type 'string
  :group 'dida)

(defcustom dida-api-timeout 20
  "单次 API 请求的超时时间（秒）。"
  :type 'integer
  :group 'dida)

(defcustom dida-api-connect-timeout 5
  "API 连接超时时间（秒）。"
  :type 'integer
  :group 'dida)

(defcustom dida-api-max-retries 2
  "幂等请求遇到 429/5xx/网络错误时的最大重试次数。"
  :type 'integer
  :group 'dida)

(defcustom dida-api-backoff-base 0.5
  "重试退避的基数（秒），按指数增长。"
  :type 'float
  :group 'dida)

(defcustom dida-api-completed-window-days 7
  "拉取已完成任务时每次切片的天数（接口单次最多 200 条）。"
  :type 'integer
  :group 'dida)

(defcustom dida-api-verify-writes t
  "写操作后是否回读校验（尤其是 complete 在 dida365 可能返回 200 但未生效）。"
  :type 'boolean
  :group 'dida)

(defcustom dida-api-log-function nil
  "API 日志函数，接受 format string 和参数；nil 表示不记录。"
  :type '(choice (const nil) function)
  :group 'dida)

(defcustom dida-api-transport-function #'dida-api--plz
  "底层 HTTP transport，签名 (METHOD URL &rest ARGS)，返回 `plz-response'。
测试时可注入假实现。"
  :type 'function
  :group 'dida)

(defcustom dida-time-zone-offset "+0800"
  "写入 dueDate 时使用的时区偏移，需与用户所在时区一致。"
  :type 'string
  :group 'dida)

;; 运行时状态
(defvar dida-token nil
  "OAuth token plist（:access_token / :refresh_token / :expires_in / :created_at），
也可以是个人 API token 字符串。")

(defvar dida-access-token ""
  "当前 access token 字符串，由 `dida-auth--ensure' 维护。")

(defvar dida-http-server nil
  "dida redirect HTTP 服务器进程。")

;;; ---------------------------------------------------------------------------
;;; 2. 错误类型
;;; ---------------------------------------------------------------------------

(define-error 'dida-api-error "Dida API 请求失败")

(defun dida-api--error-data (kind &rest plist)
  "构造 `dida-api-error' 的数据：KIND 加额外 PLIST。"
  (append (list :kind kind) plist))

(defun dida-api--signal (kind &rest plist)
  "抛出 `dida-api-error'，KIND 加额外 PLIST。"
  (signal 'dida-api-error (list (apply #'dida-api--error-data kind plist))))

(defun dida-api-error-data (err)
  "返回错误 ERR 的 plist 数据。"
  (cadr err))

(defun dida-api-error-kind (err)
  "返回错误 ERR 的 kind。"
  (plist-get (dida-api-error-data err) :kind))

(defun dida-api-error-status (err)
  "返回错误 ERR 的 HTTP status。"
  (plist-get (dida-api-error-data err) :status))

(defun dida-api-error-message (err)
  "返回错误 ERR 的可读信息。"
  (let ((data (dida-api-error-data err)))
    (or (plist-get data :message)
        (format "Dida API 错误（%s%s）"
                (plist-get data :kind)
                (if-let ((status (plist-get data :status)))
                    (format " %s" status)
                  "")))))

(defun dida-api-error-retryable-p (err)
  "错误 ERR 是否属于可重试类型。"
  (memq (dida-api-error-kind err) '(network rate-limit server)))

(defun dida-api-error-not-found-p (err)
  "错误 ERR 是否表示资源不存在。"
  (eq (dida-api-error-kind err) 'not-found))

;;; ---------------------------------------------------------------------------
;;; 3. 日志
;;; ---------------------------------------------------------------------------

(defun dida-api--log (format-string &rest args)
  "通过 `dida-api-log-function' 记录日志。"
  (when dida-api-log-function
    (apply dida-api-log-function format-string args)))

;;; ---------------------------------------------------------------------------
;;; 4. 认证
;;; ---------------------------------------------------------------------------

(defun dida-auth--client-secret ()
  "返回 client secret，兼容函数形式。"
  (let ((secret dida-client-secret))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun dida-auth ( )
  "返回 Basic Auth 头内容。"
  (concat
   "Basic "
   (base64-encode-string
    (concat dida-client-id ":" (dida-auth--client-secret)) t)))

(defun dida-auth--basic-header ()
  "返回 Authorization Basic 头。"
  (cons "Authorization" (dida-auth)))

(defun dida-auth--parse-token (body)
  "把 BODY 解析成 token plist，保留 :access_token 等关键字。"
  (when (and body (not (string-empty-p (string-trim body))))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-key-type 'keyword)
            (json-false :json-false)
            (json-null :null))
        (json-read)))))

(defun dida-auth--token-request (params)
  "向 OAuth token 端点发送 PARAMS，成功返回 token plist，失败返回 nil。
网络错误会抛出 `dida-api-error'。"
  (let* ((form (mapconcat
                (lambda (kv)
                  (concat (url-hexify-string (format "%s" (car kv)))
                          "="
                          (url-hexify-string (format "%s" (cdr kv)))))
                params "&"))
         (response (dida-api--http
                    "POST" (concat dida-oauth-base "/oauth/token")
                    :headers (list (dida-auth--basic-header)
                                   (cons "Content-Type" "application/x-www-form-urlencoded"))
                    :body form
                    :body-type 'text
                    :timeout dida-api-timeout
                    :connect-timeout dida-api-connect-timeout))
         (status (plz-response-status response))
         (body (plz-response-body response)))
    (cond
     ((and (numberp status) (<= 200 status 299))
      (dida-auth--parse-token body))
     ((memq status '(400 401 403)) nil)
     (t (dida-api--signal
         (if (and (numberp status) (>= status 500)) 'server 'client)
         :status status
         :message (format "OAuth token 请求失败（HTTP %s）" status))))))

(defun dida-auth--save ()
  "保存 `dida-token' 到 `dida-token-file'，权限 0600。"
  (when dida-token
    (let ((dir (file-name-directory (expand-file-name dida-token-file))))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)))
    (with-temp-file dida-token-file
      (insert (prin1-to-string dida-token)))
    (ignore-errors (set-file-modes dida-token-file #o600))))

(defun dida-auth--load ()
  "从 `dida-token-file' 读取 token；文件不存在或损坏时静默置 nil。"
  (when (file-exists-p dida-token-file)
    (let (raw)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents dida-token-file)
            (setq raw (buffer-string))
            (goto-char (point-min))
            (let ((value (read (current-buffer))))
              ;; 个人 API token 可能是未加引号的裸字符串；此时 `read' 会得到符号。
              (setq dida-token
                    (if (or (stringp value) (listp value))
                        value
                      (string-trim raw)))))
        (error
         (setq dida-token (and raw (string-trim raw))))))))

(defun dida-auth--personal-token-p ()
  "`dida-token' 是否是个人 API token 字符串。"
  (and (stringp dida-token) (not (string-empty-p dida-token))))

(defun dida-auth--access-token ()
  "返回当前可用的 access token 字符串。"
  (cond
   ((dida-auth--personal-token-p) dida-token)
   ((and (stringp dida-access-token) (not (string-empty-p dida-access-token)))
    dida-access-token)
   ((plist-get dida-token :access_token))))

(defun dida--token-expired-p (token)
  "计算 TOKEN 是否过期；非 plist 或缺少字段时返回 nil。"
  (when (listp token)
    (let ((expires-in (plist-get token :expires_in))
          (created-at (plist-get token :created_at)))
      (if (and expires-in created-at)
          (> (float-time) (+ created-at expires-in -30))
        nil))))

(defun dida-auth--refresh ()
  "用 refresh_token 刷新 token。成功返回 t；授权失效返回 nil；网络错误抛出。"
  (let ((refresh-token (and (listp dida-token)
                            (plist-get dida-token :refresh_token))))
    (when (and (stringp refresh-token) (not (string-empty-p refresh-token)))
      (let ((data (dida-auth--token-request
                   `(("grant_type" . "refresh_token")
                     ("refresh_token" . ,refresh-token)
                     ("redirect_uri" . ,dida-redirect-uri)
                     ("scope" . ,dida-auth-scopes)))))
        (when (and data (plist-get data :access_token))
          (unless (plist-get data :refresh_token)
            (setq data (plist-put data :refresh_token refresh-token)))
          (setq data (plist-put data :created_at (float-time)))
          (setq dida-token data
                dida-access-token (plist-get data :access_token))
          (dida-auth--save)
          (dida-api--log "token 已刷新")
          t)))))

(defun dida-auth--ensure ()
  "确保有可用 token；返回 t，或没有可用凭据时返回 nil。"
  (unless (or dida-token
              (and (stringp dida-access-token)
                   (not (string-empty-p dida-access-token))))
    (dida-auth--load))
  (cond
   ((dida-auth--personal-token-p)
    (setq dida-access-token dida-token)
    t)
   ((and (null dida-token)
         (stringp dida-access-token)
         (not (string-empty-p dida-access-token)))
    t)
   ((and (plist-get dida-token :access_token)
         (not (dida--token-expired-p dida-token)))
    (setq dida-access-token (plist-get dida-token :access_token))
    t)
   ((dida-auth--refresh) t)
   (t nil)))

;;; ---------------------------------------------------------------------------
;;; 5. HTTP 请求原语
;;; ---------------------------------------------------------------------------

(defun dida-api--plz-method (method)
  "把 METHOD 转成 `plz' 要求的符号形式：\"GET\" -> `get'。
`dida-api--request' 内部统一用字符串方法名（便于测试注入 fake transport），
而 `plz' 内部对符号方法名做 `symbol-name'、并用小写符号判断 POST/PUT/PATCH，
因此只在本适配层做转换。"
  (if (symbolp method)
      (intern (downcase (symbol-name method)))
    (intern (downcase (format "%s" method)))))

(defun dida-api--plz (method url &rest args)
  "默认 transport：用 plz 同步请求，返回 `plz-response'。
非 2xx 的 HTTP 响应也返回 response，交给上层判断；网络错误抛 `dida-api-error'。"
  (condition-case err
      (apply #'plz (dida-api--plz-method method) url :as 'response :then 'sync args)
    (plz-error
     ;; plz 的 signal 形式是 (plz-http-error "HTTP error" PLZ-ERROR)，
     ;; 部分错误则只有一段消息文本；所以不能固定取 (cadr err)。
     (let* ((data (cl-find-if #'plz-error-p (cdr err)))
            (response (and data (plz-error-response data))))
       (or response
           (dida-api--signal
            'network
            :method method
            :url url
            :message (or (and data (plz-error-message data))
                         (error-message-string err))))))))

(defun dida-api--http (method url &rest args)
  "调用 `dida-api-transport-function' 发起请求。"
  (apply dida-api-transport-function method url args))

(defun dida-api--parse-body (body)
  "严格解析响应 BODY；空 body 返回 nil。"
  (let ((trimmed (and body (string-trim body))))
    (when (and trimmed (not (string-empty-p trimmed)))
      (condition-case err
          (with-temp-buffer
            (insert trimmed)
            (goto-char (point-min))
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol)
                  (json-false :json-false)
                  ;; JSON null 归一为 nil，避免 :null 符号泄漏到上层。
                  (json-null nil))
              (let ((data (json-read)))
                (skip-chars-forward " \t\r\n")
                (unless (eobp)
                  (dida-api--signal 'parse
                                    :message "响应 JSON 后存在多余数据"))
                data)))
        (dida-api-error (signal (car err) (cdr err)))
        (error
         (dida-api--signal 'parse
                           :message (format "响应 JSON 解析失败：%s"
                                            (error-message-string err))))))))

(defun dida-api--status-kind (status)
  "把 HTTP STATUS 映射为错误 kind。"
  (cond
   ((= status 401) 'auth)
   ((= status 404) 'not-found)
   ((= status 429) 'rate-limit)
   ((and (>= status 500) (< status 600)) 'server)
   (t 'client)))

(defun dida-api--backoff (remaining &optional hint)
  "按剩余次数 REMAINING 指数退避。"
  (let ((delay (* dida-api-backoff-base
                  (expt 2 (max 0 (- dida-api-max-retries remaining))))))
    (dida-api--log "重试（%s），等待 %.2f 秒" (or hint "error") delay)
    (sleep-for delay)))

(defun dida-api--request-headers ()
  "构造 API 请求头。"
  (list (cons "Authorization" (concat "Bearer " (or (dida-auth--access-token) "")))
        (cons "Content-Type" "application/json")
        (cons "Accept" "application/json")))

(defun dida-api--truncate (string limit)
  "截断 STRING 到 LIMIT 字符。"
  (if (and string (> (length string) limit))
      (concat (substring string 0 limit) "...")
    string))

(cl-defun dida-api--request (method path &key body query (idempotent t)
                                           (retries dida-api-max-retries))
  "向 PATH 发起请求并返回解析后的数据；空响应返回 nil。

BODY 会被 `json-encode'；QUERY 是 alist。
非 2xx 时抛出 `dida-api-error'，并根据 401/429/5xx/网络错误自动处理。
IDEMPOTENT 为 nil 时（如 create）不会自动重试。"
  (unless (dida-auth--ensure)
    (dida-api--signal 'auth :message "未授权：请先运行 M-x dida-authorize"))
  (let* ((url (concat dida-api-base path
                      (if query
                          (concat "?" (url-build-query-string query))
                        "")))
         (payload (and body (json-encode body)))
         (remaining (or retries dida-api-max-retries))
         (auth-retried nil))
    (catch 'done
      (while t
        (let* ((response
                (condition-case err
                    (let ((args (list :headers (dida-api--request-headers)
                                      :timeout dida-api-timeout
                                      :connect-timeout dida-api-connect-timeout)))
                      (when payload
                        (setq args (append args (list :body payload
                                                      :body-type 'text))))
                      (apply #'dida-api--http method url args))
                  (dida-api-error
                   (if (and idempotent (> remaining 0)
                            (eq (dida-api-error-kind err) 'network))
                       (progn
                         (setq remaining (1- remaining))
                         (dida-api--backoff remaining 'network)
                         'retry)
                     (signal (car err) (cdr err)))))))
          (if (eq response 'retry)
              nil
            (let ((status (plz-response-status response))
                  (body (plz-response-body response)))
              (dida-api--log "%s %s -> %s" method path status)
              (cond
               ((not (numberp status))
                (dida-api--signal 'network
                                  :method method :path path
                                  :message "响应缺少 HTTP status"))
               ((<= 200 status 299)
                (throw 'done (dida-api--parse-body body)))
               ((= status 401)
                (if (and (not auth-retried)
                         (dida-auth--refresh))
                    (setq auth-retried t)
                  (dida-api--signal
                   'auth :status status
                   :message "认证失败，请重新运行 M-x dida-authorize")))
               ((and idempotent (> remaining 0)
                     (or (= status 429)
                         (and (>= status 500) (< status 600))))
                (setq remaining (1- remaining))
                (dida-api--backoff remaining status))
               (t
                (dida-api--signal
                 (dida-api--status-kind status)
                 :status status
                 :method method
                 :path path
                 :body (dida-api--truncate body 500)
                 :message (format "%s %s -> HTTP %s" method path status)))))))))))

;;; ---------------------------------------------------------------------------
;;; 6. 资源函数：项目
;;; ---------------------------------------------------------------------------

(cl-defun dida-api-list-projects (&key offset limit)
  "列出全部项目（自动翻页），返回项目 alist 的列表。"
  (let ((offset (or offset 0))
        (limit (or limit 200))
        (out nil)
        (seen nil)
        (guard 0))
    (while (< guard 20)
      (let* ((page (dida-api--request
                    "GET" "/open/v1/project"
                    :query (list (list "offset" (number-to-string offset))
                                 (list "limit" (number-to-string limit)))))
             (ids (mapcar (lambda (project) (alist-get 'id project)) page)))
        (cond
         ((null page)
          (setq guard 20))
         (t
          (setq out (append out page))
          (if (or (< (length page) limit)
                  (cl-every (lambda (id) (member id seen)) ids))
              (setq guard 20)
            (setq seen (append seen ids)
                  offset (+ offset limit)
                  guard (1+ guard)))))))
    ;; 防御服务端忽略分页参数导致的重复
    (let (ids result)
      (dolist (project out)
        (let ((id (alist-get 'id project)))
          (unless (member id ids)
            (push id ids)
            (push project result))))
      (nreverse result))))

(defun dida-api-get-project (pid)
  "获取项目 PID 的信息。"
  (dida-api--request "GET" (format "/open/v1/project/%s" pid)))

(defun dida-api-get-project-data (pid)
  "获取项目 PID 的数据（只含未完成任务）。返回 alist：project / tasks / columns。"
  (let ((data (dida-api--request "GET" (format "/open/v1/project/%s/data" pid))))
    (unless (and (listp data) (assoc 'tasks data))
      (dida-api--signal 'parse
                        :message (format "项目 %s 的数据缺少 tasks" pid)))
    data))

(cl-defun dida-api-create-project (name &key color sort-order viewmode kind)
  "创建项目。"
  (dida-api--request
   "POST" "/open/v1/project"
   :idempotent nil
   :body (append (list (cons 'name name))
                 (when color (list (cons 'color color)))
                 (when sort-order (list (cons 'sortOrder sort-order)))
                 (when viewmode (list (cons 'viewMode viewmode)))
                 (when kind (list (cons 'kind kind))))))

(cl-defun dida-api-update-project (pid &key name color sort-order viewmode kind)
  "更新项目 PID。"
  (dida-api--request
   "POST" (format "/open/v1/project/%s" pid)
   :body (append (when name (list (cons 'name name)))
                 (when color (list (cons 'color color)))
                 (when sort-order (list (cons 'sortOrder sort-order)))
                 (when viewmode (list (cons 'viewMode viewmode)))
                 (when kind (list (cons 'kind kind))))))

(defun dida-api-delete-project (pid)
  "删除项目 PID。"
  (condition-case err
      (progn
        (dida-api--request "DELETE" (format "/open/v1/project/%s" pid))
        t)
    (dida-api-error
     (if (dida-api-error-not-found-p err) t
       (signal (car err) (cdr err))))))

;;; ---------------------------------------------------------------------------
;;; 7. 资源函数：任务
;;; ---------------------------------------------------------------------------

(defun dida-api-get-task (pid tid)
  "获取项目 PID 下的任务 TID；不存在时返回 nil。"
  (condition-case err
      (dida-api--request
       "GET" (format "/open/v1/project/%s/task/%s" pid tid))
    (dida-api-error
     (if (dida-api-error-not-found-p err) nil
       (signal (car err) (cdr err))))))

(defun dida-api-create-task (payload)
  "创建任务。PAYLOAD 必须是完整 JSON 对象。返回创建后的 task alist 或 nil。"
  (let ((task (dida-api--request
               "POST" "/open/v1/task"
               :body payload
               :idempotent nil)))
    (cond
     ((null task)
      (dida-api--log "创建任务成功，但响应为空，无法取得 task id")
      nil)
     ((not (alist-get 'id task))
      (dida-api--signal 'parse :message "创建任务响应缺少 id"))
     (t task))))

(defun dida-api-update-task (tid payload)
  "更新任务 TID。PAYLOAD 必须完整（接口是整体替换）。"
  (dida-api--request
   "POST" (format "/open/v1/task/%s" tid)
   :body payload))

(defun dida-api-complete-task (pid tid)
  "完成任务；成功返回 t，任务不存在返回 nil。"
  (condition-case err
      (progn
        (dida-api--request
         "POST" (format "/open/v1/project/%s/task/%s/complete" pid tid))
        t)
    (dida-api-error
     (if (dida-api-error-not-found-p err) nil
       (signal (car err) (cdr err))))))

(defun dida-api-delete-task (pid tid)
  "删除任务；404 视为已删除，返回 t。"
  (condition-case err
      (progn
        (dida-api--request
         "DELETE" (format "/open/v1/project/%s/task/%s" pid tid))
        t)
    (dida-api-error
     (if (dida-api-error-not-found-p err) t
       (signal (car err) (cdr err))))))

(defun dida-api-move-task (tid from-pid to-pid)
  "把任务 TID 从 FROM-PID 移到 TO-PID。"
  (dida-api--request
   "POST" "/open/v1/task/move"
   :idempotent nil
   :body (vector `((fromProjectId . ,from-pid)
                   (toProjectId . ,to-pid)
                   (taskId . ,tid)))))

(defun dida-api-complete-tasks (pid tids)
  "批量完成任务，返回成功完成的 id 列表。"
  (dida-api--request
   "POST" "/open/v1/task/completeTasks"
   :body (list (cons 'projectId pid)
               (cons 'taskIds (vconcat tids)))))

(cl-defun dida-api-batch-tasks (&key add update)
  "批量创建/更新任务，ADD / UPDATE 是 payload 列表。
返回 (id2etag . id2error)。含 add 时不会自动重试，避免重复创建。"
  (let* ((add-vector (or (vconcat add) []))
         (update-vector (or (vconcat update) []))
         (response (dida-api--request
                    "POST" "/open/v1/task/batch"
                    :idempotent (zerop (length add-vector))
                    :body (list (cons 'add add-vector)
                                (cons 'update update-vector)))))
    (when response
      (cons (alist-get 'id2etag response)
            (alist-get 'id2error response)))))

(cl-defun dida-api-list-completed (&key project-ids start end)
  "列出已完成任务（单次最多 200 条）。
START/END 是 completedTime 的时间字符串。"
  (let ((body (append (when project-ids
                        (list (cons 'projectIds (vconcat project-ids))))
                      (when start (list (cons 'startDate start)))
                      (when end (list (cons 'endDate end))))))
    (dida-api--request "POST" "/open/v1/task/completed"
                       :body (or body (make-hash-table)))))

(cl-defun dida-api-list-completed-all (&key project-ids start end
                                             (window-days dida-api-completed-window-days))
  "按 WINDOW-DAYS 切片拉取 START 到 END 的已完成任务并去重，绕过单次 200 条上限。"
  (unless (and start end)
    (dida-api--signal 'client :message "completed 查询必须提供 start 和 end"))
  (let ((cursor (date-to-time start))
        (end-time (date-to-time end))
        (window (* 24 3600 (max 1 window-days)))
        (result nil)
        (guard 0))
    (while (and (time-less-p cursor end-time) (< guard 1000))
      (setq guard (1+ guard))
      (let* ((next (time-add cursor window))
             (chunk-end (if (time-less-p next end-time) next end-time))
             (chunk (dida-api-list-completed
                     :project-ids project-ids
                     :start (format-time-string "%Y-%m-%dT%H:%M:%S%z" cursor)
                     :end (format-time-string "%Y-%m-%dT%H:%M:%S%z" chunk-end))))
        (setq result (append result chunk))
        (setq cursor next)))
    (let (ids out)
      (dolist (task result)
        (let ((id (alist-get 'id task)))
          (unless (member id ids)
            (push id ids)
            (push task out))))
      (nreverse out))))

(cl-defun dida-api-filter-tasks (&key project-ids start end priority tag kind status)
  "按条件过滤任务（单次最多 200 条）。"
  (let ((body nil))
    (when project-ids (push (cons 'projectIds (vconcat project-ids)) body))
    (when start (push (cons 'startDate start) body))
    (when end (push (cons 'endDate end) body))
    (when priority (push (cons 'priority (vconcat priority)) body))
    (when tag (push (cons 'tag (vconcat tag)) body))
    (when kind (push (cons 'kind (vconcat kind)) body))
    (when status (push (cons 'status (vconcat status)) body))
    (dida-api--request
     "POST" "/open/v1/task/filter"
     :body (or body (make-hash-table)))))

(cl-defun dida-api-undone-tasks (&key project-ids task-ids start end)
  "列出未完成任务（start/end 必填，窗口最多 14 天）。"
  (unless (and start end)
    (dida-api--signal 'client :message "undone 查询必须提供 start 和 end"))
  (dida-api--request
   "POST" "/open/v1/task/undone"
   :body (append (when project-ids
                   (list (cons 'projectIds (vconcat project-ids))))
                 (when task-ids
                   (list (cons 'taskIds (vconcat task-ids))))
                 (list (cons 'startDate start))
                 (list (cons 'endDate end)))))

(cl-defun dida-api-search-tasks (keywords &key project-ids tags status due-from due-to)
  "按关键词搜索任务。"
  (dida-api--request
   "POST" "/open/v1/task/search"
   :body (append (list (cons 'keywords (or keywords "")))
                 (when project-ids
                   (list (cons 'projectIds (vconcat project-ids))))
                 (when tags (list (cons 'tags (vconcat tags))))
                 (when status (list (cons 'status (vconcat status))))
                 (when due-from (list (cons 'dueFrom due-from)))
                 (when due-to (list (cons 'dueTo due-to))))))

(defun dida-api-get-preference ()
  "获取用户偏好（主要是 timeZone）。"
  (dida-api--request "POST" "/open/v1/preference"))

;;; ---------------------------------------------------------------------------
;;; 8. Payload 构造
;;; ---------------------------------------------------------------------------

(defconst dida-api-preserved-fields
  '(:desc :startDate :timeZone :tags :items :reminders :repeatFlag :sortOrder)
  "update 整体替换时需要从远端原样保留的字段。")

(defun dida-api--as-vector (value)
  "把列表 VALUE 转成 vector，保证空数组编码为 [] 而不是 null。"
  (cond
   ((vectorp value) value)
   ((listp value) (vconcat value))
   (t value)))

(defun dida-api-task-payload (&rest plist)
  "由关键字 PLIST 构造任务 payload。

支持的关键字：:id :title :project-id :content :desc :is-all-day
:start-date :due-date :time-zone :reminders :tags :repeat-flag
:priority :sort-order :items :status。未提供的字段不会出现在 payload 里。"
  (let ((payload nil))
    (cl-labels ((put (key value)
                  (setq payload (plist-put payload key value))))
      (when (plist-get plist :id) (put :id (plist-get plist :id)))
      (when (plist-get plist :title) (put :title (plist-get plist :title)))
      (when (plist-get plist :project-id)
        (put :projectId (plist-get plist :project-id)))
      (when (plist-get plist :content)
        (put :content (plist-get plist :content)))
      (when (plist-get plist :desc)
        (put :desc (plist-get plist :desc)))
      (when (memq (plist-get plist :is-all-day) '(t :json-false))
        (put :isAllDay (plist-get plist :is-all-day)))
      (when (plist-get plist :start-date)
        (put :startDate (plist-get plist :start-date)))
      (when (plist-get plist :due-date)
        (put :dueDate (plist-get plist :due-date)))
      (when (plist-get plist :time-zone)
        (put :timeZone (plist-get plist :time-zone)))
      (when (plist-get plist :reminders)
        (put :reminders (dida-api--as-vector (plist-get plist :reminders))))
      (when (plist-get plist :tags)
        (put :tags (dida-api--as-vector (plist-get plist :tags))))
      (when (plist-get plist :repeat-flag)
        (put :repeatFlag (plist-get plist :repeat-flag)))
      (when (integerp (plist-get plist :priority))
        (put :priority (plist-get plist :priority)))
      (when (integerp (plist-get plist :sort-order))
        (put :sortOrder (plist-get plist :sort-order)))
      (when (plist-get plist :items)
        (put :items (dida-api--as-vector (plist-get plist :items))))
      (when (integerp (plist-get plist :status))
        (put :status (plist-get plist :status))))
    payload))

(defun dida-api-preserve-extras (payload remote)
  "把 REMOTE 中 `dida-api-preserved-fields' 的字段补进 PAYLOAD。
REMOTE 的键是不带冒号的符号（如 `startDate'），PAYLOAD 的键是关键字。
PAYLOAD 已显式提供的字段优先。"
  (dolist (key dida-api-preserved-fields)
    (let* ((remote-key (intern (substring (symbol-name key) 1)))
           (cell (assoc remote-key remote)))
      (when (and cell (not (plist-member payload key)))
        (setq payload
              (plist-put payload key
                         (if (memq key '(:tags :items :reminders))
                             (dida-api--as-vector (cdr cell))
                           (cdr cell)))))))
  payload)

(defun dida-api-update-payload (remote payload)
  "整体替换语义下的完整 payload：自己的字段 + 远端未映射字段。"
  (dida-api-preserve-extras payload remote))

(defun dida-api-payload-from-remote (remote &rest overrides)
  "从远端 REMOTE 构造完整 update payload，OVERLAYS 覆盖指定字段。"
  (let ((payload
         (dida-api-task-payload
          :id (alist-get 'id remote)
          :title (alist-get 'title remote)
          :project-id (alist-get 'projectId remote)
          :content (or (alist-get 'content remote) "")
          :desc (alist-get 'desc remote)
          :is-all-day (if (eq (alist-get 'isAllDay remote) t) t :json-false)
          :start-date (alist-get 'startDate remote)
          :due-date (alist-get 'dueDate remote)
          :time-zone (alist-get 'timeZone remote)
          :repeat-flag (alist-get 'repeatFlag remote)
          :priority (or (alist-get 'priority remote) 0)
          :sort-order (alist-get 'sortOrder remote)
          :status (or (alist-get 'status remote) 0))))
    (setq payload (dida-api-preserve-extras payload remote))
    (cl-loop for (key value) on overrides by #'cddr
             do (setq payload (plist-put payload key value)))
    payload))

;;; ---------------------------------------------------------------------------
;;; 9. OAuth 授权流程
;;; ---------------------------------------------------------------------------

(defun dida-start-http-server (port)
  "在 PORT 上启动本地 HTTP 服务器，用于接收 OAuth code。"
  (setq dida-http-server
        (make-network-process
         :name "dida-http-server"
         :buffer "*dida-http-server*"
         :family 'ipv4
         :service port
         :host 'local
         :server t
         :sentinel 'dida-server-sentinel
         :filter 'dida-http-server-filter)))

(defun dida-server-sentinel (_proc _msg)
  "HTTP 服务器哨兵（当前不需要处理）。")

(defun dida-get-code-string (query)
  "从 QUERY 中提取 code 并放入 kill-ring，返回 code。"
  (when query
    (let* ((params (ignore-errors (url-parse-query-string query t)))
           (code (and params (cadr (assoc "code" params)))))
      (when (and (stringp code) (not (string-empty-p code)))
        (kill-new code)
        code))))

(defun dida-http-server-filter (proc data)
  "处理 OAuth 回调请求。"
  (when (string-match "\\([^ ]+\\) HTTP/1.[01]" data)
    (let* ((uri (match-string 1 data))
           (components (split-string uri "\\?"))
           (query (if (cdr components) (cadr components) nil))
           (code (dida-get-code-string query))
           (response-body
            (if code
                "Code copied to kill-ring, please return to Emacs and yank\n"
              "Missing code\n")))
      (process-send-string
       proc
       (format "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: %d\r\n\r\n%s"
               (length response-body)
               response-body))
      (delete-process proc))))

(defun dida-stop-http-server ()
  "停止 OAuth 回调服务器。"
  (interactive)
  (when (process-live-p dida-http-server)
    (delete-process dida-http-server)
    (setq dida-http-server nil)))

(defun dida--exchange-code-for-token (code)
  "把输入的 CODE 交换为 token 并保存，返回 token plist。"
  (let ((data (dida-auth--token-request
               `(("grant_type" . "authorization_code")
                 ("code" . ,code)
                 ("redirect_uri" . ,dida-redirect-uri)
                 ("scope" . ,dida-auth-scopes)))))
    (when (and data (plist-get data :access_token))
      (setq data (plist-put data :created_at (float-time)))
      (setq dida-token data
            dida-access-token (plist-get data :access_token))
      (dida-auth--save)
      data)))

(defun dida-authorize ()
  "获取 dida 的验证 token。"
  (interactive)
  (when (or (string-empty-p dida-client-id)
            (string-empty-p (dida-auth--client-secret)))
    (user-error "请先设置 dida-client-id 和 dida-client-secret"))
  (let* ((state (format "%06x" (random (expt 16 6))))
         (auth-url (concat dida-oauth-base "/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,dida-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,dida-redirect-uri)
                              ("scope" ,dida-auth-scopes)
                              ("state" ,state))))))
    (dida-start-http-server dida-oauth-port)
    (unwind-protect
        (progn
          (browse-url auth-url)
          (let ((code (read-string "请粘贴 code: ")))
            (when (string-empty-p code)
              (user-error "没有拿到 code"))
            (if (dida--exchange-code-for-token code)
                (message "验证成功！")
              (message "获取 token 失败."))))
      (dida-stop-http-server))))

(defun dida--check-token ()
  "检查 token 是否可用；需要时自动刷新。可用返回 t，无凭据返回 nil。"
  (dida-auth--ensure))

(provide 'dida)
;;; dida.el ends here
