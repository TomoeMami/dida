;;; dida.el --- Dida365 同步的插件 -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2025.06
;; Version: 2.0.0

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

;; 与滴答清单(Dida365)同步的插件。
;;
;; 本文件包含三层：
;;
;; 1. API 层（dida-api-* / dida-auth-*）：
;;    - 统一的 HTTP 请求原语：认证、401 刷新重试、429/5xx 退避、严格 JSON 解析；
;;    - 纯数据的资源函数：项目、任务、已完成窗口、过滤、搜索、批量、移动、删除；
;;    - 错误统一为 `dida-api-error'，带 :kind / :status，可判断是否可重试。
;;
;; 2. 兼容层：
;;    - 旧的 dida-get-* / dida-create-task / dida-update-task 等保留原签名，
;;      内部改为调用新的 API 层，现有 capture / refile 配置无需修改。
;;
;; 3. Org / 命令层：
;;    - Org ↔ Task 的字段映射、fetch / push / create / update 等命令。
;;
;; 设计约定：API 层不碰 Org、不做同步决策；上层只依赖纯数据和明确错误。

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'button)
(require 'org)
(require 'org-element)

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

(defun dida-api--plz (method url &rest args)
  "默认 transport：用 plz 同步请求，返回 `plz-response'。
非 2xx 的 HTTP 响应也返回 response，交给上层判断；网络错误抛 `dida-api-error'。"
  (condition-case err
      (apply #'plz method url :as 'response :then 'sync args)
    (plz-error
     (let* ((data (cadr err))
            (response (and (plz-error-p data)
                           (plz-error-response data))))
       (or response
           (dida-api--signal
            'network
            :method method
            :url url
            :message (or (and (plz-error-p data)
                              (or (plz-error-message data)
                                  (error-message-string err)))
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
;;; 9. 旧 API 兼容层
;;; ---------------------------------------------------------------------------

(defun dida-get-task-by-pid-tid (pid tid)
  "根据项目 ID 和任务 ID 获取任务；不存在时返回 nil。"
  (dida-api-get-task pid tid))

(defun dida-get-user-project ()
  "获取用户全部项目（自动翻页）。"
  (dida-api-list-projects))

(defun dida-get-project-by-id (pid)
  "根据 PID 获取项目信息。"
  (dida-api-get-project pid))

(defun dida-get-project-by-id-with-data (pid)
  "根据 PID 获取项目及其未完成任务。"
  (dida-api-get-project-data pid))

(defun dida--org-time-to-api (org-time)
  "把 org timestamp ORG-TIME 转成 API 的 dueDate / isAllDay 字段。"
  (when org-time
    (let* ((raw (plist-get (cadr org-time) :raw-value))
           (timed (and raw (string-match-p "[0-9]:[0-9]" raw)))
           (all-day (not timed))
           (format-string (if all-day
                              (concat "%Y-%m-%dT00:00:00" dida-time-zone-offset)
                            (concat "%Y-%m-%dT%H:%M:00" dida-time-zone-offset))))
      (list :dueDate (format-time-string format-string
                                         (org-timestamp-to-time org-time))
            :isAllDay (if all-day t :json-false)))))

(cl-defun dida-create-task (title pid &optional &key content org-time
                                      (priority 0) repeatflag)
  "创建任务；返回创建后的 task alist（可能为 nil，见 `dida-api-create-task'）。"
  (let* ((time (dida--org-time-to-api org-time))
         (all-day (plist-get time :isAllDay))
         (payload (dida-api-task-payload
                   :title title
                   :project-id pid
                   :content content
                   :priority priority
                   :repeat-flag repeatflag
                   :due-date (plist-get time :dueDate)
                   :is-all-day (if time all-day :json-false))))
    (when time
      ;; 保留旧行为：新建任务带默认提醒
      (setq payload
            (plist-put payload :reminders
                       (if (eq all-day t)
                           (vector "TRIGGER:P0DT9H0M0S")
                         (vector "TRIGGER:PT0S")))))
    (dida-api-create-task payload)))

(cl-defun dida-update-task (pid tid &optional &key title content org-time
                                       priority status repeatflag)
  "更新任务（整体替换，但会保留远端未映射字段）。

STATUS 为 nil 时保留远端状态；远端已完成而目标不是已完成时拒绝重开。"
  (let* ((remote (dida-api-get-task pid tid))
         (remote-status (or (and remote (alist-get 'status remote)) 0))
         (target-status (or status remote-status)))
    (unless remote
      (dida-api--signal 'not-found :status 404
                        :message (format "任务 %s 不存在" tid)))
    (when (and (= remote-status 2) (/= target-status 2))
      (dida-api--signal 'client
                        :message "滴答不支持重开已完成任务；请先在滴答中处理"))
    (let ((payload (dida-api-payload-from-remote remote)))
      (when title
        (setq payload (plist-put payload :title title)))
      (setq payload (plist-put payload :content (or content "")))
      (when (integerp priority)
        (setq payload (plist-put payload :priority priority)))
      (when (integerp status)
        (setq payload (plist-put payload :status status)))
      (if org-time
          (let ((time (dida--org-time-to-api org-time)))
            (setq payload (plist-put payload :dueDate (plist-get time :dueDate)))
            (setq payload (plist-put payload :isAllDay (plist-get time :isAllDay))))
        (setq payload (plist-put payload :dueDate nil)))
      (setq payload (plist-put payload :repeatFlag repeatflag))
      (dida-api-update-task tid payload))))

(defun dida-complete-task (pid tid)
  "完成任务；必要时回读并用 status=2 兜底。返回 t。"
  (let ((first (dida-api-complete-task pid tid)))
    (unless dida-api-verify-writes
      (cl-return-from dida-complete-task (and first t)))
    (let ((fresh (dida-api-get-task pid tid)))
      (cond
       ((null fresh) t)
       ((= (or (alist-get 'status fresh) 0) 2) t)
       (t
        (let ((payload (dida-api-payload-from-remote fresh)))
          (setq payload (plist-put payload :status 2))
          (dida-api-update-task tid payload))
        t)))))

(defun dida-delete-task (pid tid)
  "删除任务；不存在视为成功。"
  (dida-api-delete-task pid tid))

(defun dida-complete-tasks (pid tids)
  "批量完成任务，返回成功完成的 id 列表。"
  (dida-api-complete-tasks pid tids))

(defun dida-move-task (tid from-pid to-pid)
  "把任务 TID 从 FROM-PID 移到 TO-PID。"
  (dida-api-move-task tid from-pid to-pid))

(cl-defun dida-update-project (pid &optional &key name color
                                          (viewmode "list") (kind "TASK"))
  "更新项目 PID。"
  (dida-api-update-project pid :name name :color color
                           :viewmode viewmode :kind kind))

(defun dida-delete-project (pid)
  "删除项目 PID。"
  (dida-api-delete-project pid))

;;; ---------------------------------------------------------------------------
;;; 10. OAuth 授权流程
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

;;; ---------------------------------------------------------------------------
;;; 11. Org 转换
;;; ---------------------------------------------------------------------------

(defun dida--format-task-insert-string (&rest plist)
  "将 PLIST 中的任务字段拼接成可插入的 org heading 字符串。"
  (let* ((status (plist-get plist :status))
         (priority (plist-get plist :priority))
         (title (plist-get plist :title))
         (due-date (plist-get plist :due-date))
         (isallday (plist-get plist :isallday))
         (content (plist-get plist :content))
         (id (plist-get plist :id))
         (repeatflag (plist-get plist :repeatflag))
         (deadline-or-scheduled "SCHEDULED: ")
         (tid-or-did "DIDA_TID")
         (h-m (if (eq isallday ':json-false) " %H:%M" ""))
         (repeater (if repeatflag
                       (downcase
                        (replace-regexp-in-string
                         "RRULE:FREQ=\\(.\\).+;INTERVAL=\\(.+\\)" " +\\2\\1"
                         repeatflag))
                     "")))
    (concat "** " (if (= status 2) "DONE" "TODO")
            (pcase priority
              (5 " [#A]")
              (3 " [#B]")
              (1 " [#C]")
              (_ ""))
            " " title
            (concat
             (when due-date
               (concat "\n" deadline-or-scheduled
                       (format-time-string
                        (concat "<%Y-%m-%d %a" h-m repeater ">")
                        (date-to-time due-date))))
             "\n:PROPERTIES:\n:" tid-or-did ": " id "\n:END:\n")
            (when content
              (concat "\n" content "\n")))))

(defun dida-task-to-heading (task)
  "将一项 dida task 数据转换为 org 的 heading 字符串。"
  (let* ((id (alist-get 'id task))
         (isallday (alist-get 'isAllDay task))
         (title (alist-get 'title task))
         (content (alist-get 'content task))
         (status (alist-get 'status task))
         (due-date (alist-get 'dueDate task))
         (priority (alist-get 'priority task))
         (repeatflag (alist-get 'repeatFlag task)))
    (dida--format-task-insert-string
     :status status :priority priority :title title :due-date due-date
     :isallday isallday :content content :id id :repeatflag repeatflag)))

(defun dida-heading-to-task (func)
  "将 org heading 转换为 dida task 格式，并根据 FUNC 调用。"
  (let* ((element (save-excursion
                    (org-back-to-heading t)
                    (org-element-at-point)))
         (tid (org-entry-get nil "DIDA_TID"))
         (pid (org-entry-get nil "DIDA_PID" t))
         (title (org-element-property :title element))
         (priority (pcase (org-element-property :priority element)
                     (?A 5)
                     (?B 3)
                     (?C 1)
                     (_ 0)))
         (scheduled (org-element-property :scheduled element))
         (content (when (org-element-property :contents-begin element)
                    (save-excursion
                      (org-back-to-heading t)
                      (org-end-of-meta-data t)
                      (let* ((start (point))
                             (end (org-element-property :contents-end element)))
                        (when (and end (> end start))
                          (string-trim
                           (buffer-substring-no-properties start end)))))))
         (repeatflag (when-let ((repeat-string (org-get-repeat)))
                       (concat "RRULE:FREQ="
                               (pcase (substring repeat-string -1)
                                 ("d" "DAILY;")
                                 ("w" "WEEKLY;")
                                 ("m" "MONTHLY;")
                                 ("y" "YEARLY;"))
                               "INTERVAL="
                               (when (string-match "\\([0-9]+\\)" repeat-string)
                                 (match-string 1 repeat-string))))))
    (cond
     ((eq func 'dida-update-task)
      (dida-update-task pid tid
                        :title title :content content
                        :org-time scheduled :priority priority
                        :status 0 :repeatflag repeatflag)
      (message "%s 已更新" tid))
     ((eq func 'dida-create-task)
      (let ((response (dida-create-task
                       title pid :content content :org-time scheduled
                       :priority priority :repeatflag repeatflag)))
        (if-let ((new-tid (alist-get 'id response)))
            (progn
              (org-set-property "DIDA_TID" new-tid)
              (message "%s 已创建" new-tid))
          (message "创建结果未知：已保留本地条目，请稍后同步确认"))))
     (t
      (error "'dida-heading-to-task' 的非法函数调用！")))))

;;; ---------------------------------------------------------------------------
;;; 12. 同步命令
;;; ---------------------------------------------------------------------------

(defun dida-create ()
  "根据当前 heading 创建新任务。"
  (interactive)
  (dida-heading-to-task 'dida-create-task))

(defun dida-update ()
  "根据当前 heading 更新任务。"
  (interactive)
  (dida-heading-to-task 'dida-update-task))


;;; ---------------------------------------------------------------------------
;;; 13. 同步引擎（单文件 MVP）
;;; ---------------------------------------------------------------------------

(defcustom dida-sync-delay 3
  "状态变化后延迟多少秒触发一次同步。"
  :type 'integer
  :group 'dida)

(defcustom dida-state-file
  (expand-file-name "dida-state.json" user-emacs-directory)
  "本机同步状态文件（base 缓存 / 完成 outbox / 冲突 / 墓碑）。"
  :type 'file
  :group 'dida)

(defcustom dida-done-policy 'remove
  "完成的任务在本地如何处理：remove（默认）或 keep。"
  :type '(choice (const remove) (const keep))
  :group 'dida)

(defcustom dida-archive-file nil
  "如果非 nil，移除任务前先把子树追加到这个文件；nil 表示直接移除。"
  :type '(choice (const nil) file)
  :group 'dida)

(defvar dida--state nil
  "当前同步状态 plist。")

(defvar dida--sync-timer nil
  "延迟同步的 timer。")

(defconst dida--snapshot-fields
  '(:title :content :status :priority :due :all-day :repeat)
  "参与同步比较的字段。")

(defun dida--snapshot-hash (snapshot)
  "计算 SNAPSHOT 的稳定指纹。"
  (secure-hash 'sha1
               (prin1-to-string
                (mapcar (lambda (field) (cons field (plist-get snapshot field)))
                        dida--snapshot-fields))))

(defun dida--snapshot-equal-p (a b)
  "比较两个 SNAPSHOT 是否等价。"
  (equal (dida--snapshot-hash a) (dida--snapshot-hash b)))

(defun dida--canonical-repeat (rrule)
  "把 RRULE 归一成 FREQ/INTERVAL；复杂规则原样保留。"
  (when (and (stringp rrule) (not (string-empty-p rrule)))
    (let* ((freq (and (string-match "FREQ=\\([A-Z]+\\)" rrule)
                      (match-string 1 rrule)))
           (interval (if (string-match "INTERVAL=\\([0-9]+\\)" rrule)
                         (string-to-number (match-string 1 rrule))
                       1)))
      (if (and freq
               (not (string-match-p "BY[AD]AY\\|BYMONTH\\|BYMONTHDAY" rrule)))
          (format "RRULE:FREQ=%s;INTERVAL=%d" freq interval)
        rrule))))

(defun dida--org-repeat-to-rrule (repeater)
  "把 org 的 `+2w' 重复写法转成 RRULE。"
  (when (and (stringp repeater)
             (string-match "\\+\\([0-9]+\\)\\([dwmy]\\)" repeater))
    (let ((interval (match-string 1 repeater))
          (unit (match-string 2 repeater)))
      (format "RRULE:FREQ=%s;INTERVAL=%s"
              (pcase unit
                ("d" "DAILY")
                ("w" "WEEKLY")
                ("m" "MONTHLY")
                ("y" "YEARLY"))
              interval))))

(defun dida--api-due->canonical (due all-day)
  "把 API 的 DUE 归一成本地日期/时间字符串。"
  (when (and (stringp due) (not (string-empty-p due)))
    (if all-day
        (substring due 0 10)
      (format-time-string "%Y-%m-%dT%H:%M" (date-to-time due)))))

(defun dida--org-direct-body ()
  "返回当前 heading 的直属正文（不含子 heading）。"
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((start (point))
          (end (save-excursion (outline-next-heading) (point))))
      (if (>= start end)
          ""
        (string-trim (buffer-substring-no-properties start end))))))

(defun dida--snapshot-from-org ()
  "从当前 Org heading 构造 canonical snapshot。"
  (let* ((element (org-element-at-point))
         (todo (org-element-property :todo-type element))
         (priority (pcase (org-element-property :priority element)
                     (?A 5)
                     (?B 3)
                     (?C 1)
                     (_ 0)))
         (scheduled (org-element-property :scheduled element))
         (all-day (and scheduled
                       (not (org-element-property :hour-start scheduled))))
         (due (when scheduled
                (format-time-string
                 (if all-day "%Y-%m-%d" "%Y-%m-%dT%H:%M")
                 (org-timestamp-to-time scheduled)))))
    (list :title (or (org-element-property :title element) "")
          :content (or (dida--org-direct-body) "")
          :status (if (eq todo 'done) 2 0)
          :priority priority
          :due due
          :all-day (and all-day t)
          :repeat (dida--canonical-repeat
                   (dida--org-repeat-to-rrule (org-get-repeat))))))

(defun dida--snapshot-from-api (task _pid)
  "从远端 TASK 构造 canonical snapshot。"
  (let ((all-day (eq (alist-get 'isAllDay task) t)))
    (list :title (or (alist-get 'title task) "")
          :content (or (alist-get 'content task) "")
          :status (pcase (alist-get 'status task) (2 2) (-1 2) (_ 0))
          :priority (or (alist-get 'priority task) 0)
          :due (dida--api-due->canonical (alist-get 'dueDate task) all-day)
          :all-day all-day
          :repeat (dida--canonical-repeat (alist-get 'repeatFlag task)))))

(defun dida--snapshot-due->api (snapshot)
  "把 SNAPSHOT 的 due 转成 API 的 dueDate 字符串。"
  (let ((due (plist-get snapshot :due)))
    (when (and (stringp due) (not (string-empty-p due)))
      (if (plist-get snapshot :all-day)
          (concat due "T00:00:00" dida-time-zone-offset)
        (concat due ":00" dida-time-zone-offset)))))

(defun dida--merge-snapshots (base local remote)
  "三向合并；返回 (:snapshot :conflicting-fields)。"
  (let (snapshot conflicts)
    (dolist (field dida--snapshot-fields)
      (let ((before (plist-get base field))
            (ours (plist-get local field))
            (theirs (plist-get remote field)))
        (cond ((equal ours theirs) (setq snapshot (plist-put snapshot field ours)))
              ((equal ours before) (setq snapshot (plist-put snapshot field theirs)))
              ((equal theirs before) (setq snapshot (plist-put snapshot field ours)))
              (t (push field conflicts)
                 (setq snapshot (plist-put snapshot field ours))))))
    (list :snapshot snapshot :conflicting-fields (nreverse conflicts))))

;;; --- 状态文件 ---------------------------------------------------------------

(defun dida--snapshot-to-json (snapshot)
  "SNAPSHOT -> JSON 可编码的 alist。"
  (list (cons 'title (plist-get snapshot :title))
        (cons 'content (plist-get snapshot :content))
        (cons 'status (plist-get snapshot :status))
        (cons 'priority (plist-get snapshot :priority))
        (cons 'due (plist-get snapshot :due))
        (cons 'allDay (and (plist-get snapshot :all-day) t))
        (cons 'repeat (plist-get snapshot :repeat))))

(defun dida--snapshot-from-json (alist)
  "JSON alist -> SNAPSHOT。"
  (when alist
    (list :title (or (alist-get 'title alist) "")
          :content (or (alist-get 'content alist) "")
          :status (or (alist-get 'status alist) 0)
          :priority (or (alist-get 'priority alist) 0)
          :due (alist-get 'due alist)
          :all-day (and (alist-get 'allDay alist) t)
          :repeat (alist-get 'repeat alist))))

(defun dida-state--path ()
  "返回状态文件路径。"
  (expand-file-name dida-state-file))

(defun dida-state--empty ()
  "空状态。"
  (list :version 1 :last-sync nil :bases nil :pids nil
        :pending nil :tombstones nil :conflicts nil))

(defun dida-state--read-json ()
  "读取状态 JSON。"
  (with-temp-buffer
    (insert-file-contents (dida-state--path))
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false :json-false)
          (json-null nil))
      (json-read))))

(defun dida-state--key (key)
  "JSON 读取后的动态 key 可能变成 symbol，这里统一转回 string。"
  (if (symbolp key) (symbol-name key) key))

(defun dida-state--load ()
  "加载状态到 `dida--state'。"
  (setq dida--state (dida-state--empty))
  (when (file-exists-p (dida-state--path))
    (condition-case err
        (let* ((data (dida-state--read-json))
               (bases (mapcar (lambda (pair)
                                (cons (dida-state--key (car pair))
                                      (dida--snapshot-from-json (cdr pair))))
                              (alist-get 'bases data)))
               (tombstones (mapcar (lambda (pair)
                                     (cons (dida-state--key (car pair))
                                           (dida--snapshot-from-json (cdr pair))))
                                   (alist-get 'tombstones data)))
               (conflicts
                (mapcar
                 (lambda (conflict)
                   (list :tid (alist-get 'tid conflict)
                         :pid (alist-get 'pid conflict)
                         :title (alist-get 'title conflict)
                         :reason (alist-get 'reason conflict)
                         :base (dida--snapshot-from-json (alist-get 'base conflict))
                         :local (dida--snapshot-from-json (alist-get 'local conflict))
                         :remote (dida--snapshot-from-json (alist-get 'remote conflict))))
                 (alist-get 'conflicts data))))
          (setq dida--state
                (list :version (or (alist-get 'version data) 1)
                      :last-sync (alist-get 'lastSync data)
                      :bases bases
                      :pids (mapcar (lambda (pair)
                                      (cons (dida-state--key (car pair)) (cdr pair)))
                                    (alist-get 'pids data))
                      :pending (mapcar (lambda (pair)
                                         (cons (dida-state--key (car pair)) (cdr pair)))
                                       (alist-get 'pending data))
                      :tombstones tombstones
                      :conflicts conflicts)))
      (error
       (message "dida: 状态文件损坏，已忽略：%s" (error-message-string err))
       (setq dida--state (dida-state--empty)))))
  dida--state)

(defun dida-state--save ()
  "原子写状态文件。"
  (when dida--state
    (let* ((path (dida-state--path))
           (dir (file-name-directory path)))
      (unless (file-directory-p dir) (make-directory dir t))
      (let ((temp (make-temp-file (expand-file-name ".dida-state-" dir))))
        (unwind-protect
            (progn
              (with-temp-file temp
                (insert
                 (json-encode
                  (list
                   (cons 'version (plist-get dida--state :version))
                   (cons 'lastSync (plist-get dida--state :last-sync))
                   (cons 'bases
                         (mapcar
                          (lambda (pair)
                            (cons (car pair)
                                  (dida--snapshot-to-json (cdr pair))))
                          (plist-get dida--state :bases)))
                   (cons 'pids (plist-get dida--state :pids))
                   (cons 'pending (plist-get dida--state :pending))
                   (cons 'tombstones
                         (mapcar
                          (lambda (pair)
                            (cons (car pair)
                                  (dida--snapshot-to-json (cdr pair))))
                          (plist-get dida--state :tombstones)))
                   (cons 'conflicts
                         (mapcar
                          (lambda (conflict)
                            (list (cons 'tid (plist-get conflict :tid))
                                  (cons 'pid (plist-get conflict :pid))
                                  (cons 'title (plist-get conflict :title))
                                  (cons 'reason (plist-get conflict :reason))
                                  (cons 'base
                                        (dida--snapshot-to-json
                                         (plist-get conflict :base)))
                                  (cons 'local
                                        (dida--snapshot-to-json
                                         (plist-get conflict :local)))
                                  (cons 'remote
                                        (dida--snapshot-to-json
                                         (plist-get conflict :remote)))))
                          (plist-get dida--state :conflicts)))))))
              (set-file-modes temp #o600)
              (rename-file temp path t))
          (when (file-exists-p temp) (delete-file temp)))))))

(defun dida-state--base (tid)
  "返回 TID 的 base snapshot。"
  (cdr (assoc tid (plist-get dida--state :bases))))

(defun dida-state--set-base (tid snapshot &optional pid)
  "设置 TID 的 base snapshot 与 PID。"
  (let ((bases (cl-remove tid (copy-sequence (plist-get dida--state :bases))
                          :key #'car :test #'equal)))
    (push (cons tid snapshot) bases)
    (setq dida--state (plist-put dida--state :bases bases))
    (when pid
      (let ((pids (cl-remove tid (copy-sequence (plist-get dida--state :pids))
                             :key #'car :test #'equal)))
        (push (cons tid pid) pids)
        (setq dida--state (plist-put dida--state :pids pids))))))

(defun dida-state--del-base (tid)
  "删除 TID 的 base 与 pid 记录。"
  (setq dida--state
        (plist-put dida--state :bases
                   (cl-remove tid (plist-get dida--state :bases)
                              :key #'car :test #'equal)))
  (setq dida--state
        (plist-put dida--state :pids
                   (cl-remove tid (plist-get dida--state :pids)
                              :key #'car :test #'equal))))

(defun dida-state--pid (tid)
  "返回 TID 记录的项目 id。"
  (cdr (assoc tid (plist-get dida--state :pids))))

(defun dida-state--pending-p (tid)
  "TID 是否在完成 outbox 中。"
  (assoc tid (plist-get dida--state :pending)))

(defun dida-state--add-pending (tid pid)
  "把 TID 加入完成 outbox。"
  (let ((pending (cl-remove tid (copy-sequence (plist-get dida--state :pending))
                            :key #'car :test #'equal)))
    (push (cons tid pid) pending)
    (setq dida--state (plist-put dida--state :pending pending))))

(defun dida-state--del-pending (tid)
  "从完成 outbox 移除 TID。"
  (setq dida--state
        (plist-put dida--state :pending
                   (cl-remove tid (plist-get dida--state :pending)
                              :key #'car :test #'equal))))

(defun dida-state--tombstone (tid)
  "返回 TID 的本地删除快照。"
  (cdr (assoc tid (plist-get dida--state :tombstones))))

(defun dida-state--set-tombstone (tid snapshot)
  "记录 TID 的本地删除快照。"
  (let ((tombstones (cl-remove tid (copy-sequence (plist-get dida--state :tombstones))
                               :key #'car :test #'equal)))
    (push (cons tid snapshot) tombstones)
    (setq dida--state (plist-put dida--state :tombstones tombstones))))

(defun dida-state--del-tombstone (tid)
  "删除 TID 的墓碑。"
  (setq dida--state
        (plist-put dida--state :tombstones
                   (cl-remove tid (plist-get dida--state :tombstones)
                              :key #'car :test #'equal))))

(defun dida-state--add-conflict (tid pid reason base local remote &optional title)
  "记录一个冲突。"
  (let ((conflict (list :tid tid :pid pid :reason reason :title title
                        :base base :local local :remote remote))
        (conflicts (cl-remove tid (copy-sequence (plist-get dida--state :conflicts))
                              :key (lambda (c) (plist-get c :tid)) :test #'equal)))
    (push conflict conflicts)
    (setq dida--state (plist-put dida--state :conflicts conflicts))))

(defun dida-state--remove-conflict (tid)
  "移除 TID 的冲突。"
  (setq dida--state
        (plist-put dida--state :conflicts
                   (cl-remove tid (plist-get dida--state :conflicts)
                              :key (lambda (c) (plist-get c :tid)) :test #'equal))))

;;; --- Org 读写 ---------------------------------------------------------------

(defun dida--org-file ()
  "返回同步文件路径。"
  (unless (and (stringp dida-sync-file) (not (string-empty-p dida-sync-file)))
    (user-error "请先设置 dida-sync-file"))
  (expand-file-name dida-sync-file))

(defun dida--org-buffer ()
  "返回同步文件的 buffer（不切换当前 buffer）。"
  (let ((file (dida--org-file)))
    (make-directory (file-name-directory file) t)
    (or (find-buffer-visiting file) (find-file-noselect file))))

(defun dida--org-collect ()
  "扫描同步文件，返回 (:buffer :projects :tasks)。"
  (let ((buffer (dida--org-buffer)))
    (with-current-buffer buffer
      (when (buffer-modified-p) (save-buffer))
      (org-with-wide-buffer
       (let (projects tasks seen)
         (org-map-entries
          (lambda ()
            (let ((pid (org-entry-get nil "DIDA_PID"))
                  (tid (org-entry-get nil "DIDA_TID"))
                  (level (org-current-level))
                  (title (or (org-element-property :title (org-element-at-point)) "")))
              (cond
               ((and pid (= level 1))
                (push (list :pid pid :name title) projects))
               (tid
                (when (member tid seen)
                  (user-error "Org 中 DIDA_TID 重复：%s" tid))
                (push tid seen)
                (push (list :tid tid
                            :pid (org-entry-get nil "DIDA_PID" t)
                            :title title
                            :marker (copy-marker (point))
                            :snapshot (dida--snapshot-from-org)
                            :base (org-entry-get nil "DIDA_BASE"))
                      tasks))
               ((and (= level 2)
                     (org-element-property :todo-type (org-element-at-point))
                     (org-entry-get nil "DIDA_PID" t))
                (push (list :new t
                            :pid (org-entry-get nil "DIDA_PID" t)
                            :title title
                            :marker (copy-marker (point))
                            :snapshot (dida--snapshot-from-org))
                      tasks)))))
          nil nil)
         (list :buffer buffer :projects (nreverse projects) :tasks (nreverse tasks)))))))

(defun dida--org-find-tid (tid)
  "在当前 buffer 中查找 TID 的位置。"
  (org-with-wide-buffer
   (catch 'found
     (org-map-entries
      (lambda ()
        (when (equal (org-entry-get nil "DIDA_TID") tid)
          (throw 'found (point))))
      nil nil)
     nil)))

(defun dida--org-find-pid (pid)
  "在当前 buffer 中查找项目 PID 的位置。"
  (org-with-wide-buffer
   (catch 'found
     (org-map-entries
      (lambda ()
        (when (and (= (org-current-level) 1)
                   (equal (org-entry-get nil "DIDA_PID") pid))
          (throw 'found (point))))
      nil nil)
     nil)))

(defun dida--org-ensure-project (pid name)
  "确保项目 heading 存在，返回位置。"
  (or (dida--org-find-pid pid)
      (with-current-buffer (dida--org-buffer)
        (org-with-wide-buffer
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert
          (if dida-global-tag
              (format "* %s :%s:\n:PROPERTIES:\n:DIDA_PID: %s\n:END:\n"
                      (or name pid) dida-global-tag pid)
            (format "* %s\n:PROPERTIES:\n:DIDA_PID: %s\n:END:\n"
                    (or name pid) pid)))
         (save-buffer)
         (dida--org-find-pid pid)))))

(defun dida--org-apply-snapshot (tid snapshot hash)
  "把 SNAPSHOT 应用到 TID 对应 heading，并写入 HASH。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (let ((pos (dida--org-find-tid tid)))
       (unless pos (error "找不到任务 %s" tid))
       (goto-char pos)
       (org-edit-headline (or (plist-get snapshot :title) ""))
       (org-todo (if (= (or (plist-get snapshot :status) 0) 2) "DONE" "TODO"))
       (let ((priority (plist-get snapshot :priority)))
         (if (memq priority '(5 3 1))
             (org-priority (pcase priority (5 ?A) (3 ?B) (1 ?C)))
           (when (org-element-property :priority (org-element-at-point))
             (org-priority ?\s))))
       (let ((due (plist-get snapshot :due)))
         (if (and (stringp due) (not (string-empty-p due)))
             (org-schedule nil (dida--due-for-org snapshot))
           (when (org-entry-get nil "SCHEDULED")
             (org-schedule '(4)))))
       (let ((content (or (plist-get snapshot :content) "")))
         (save-excursion
           (org-back-to-heading t)
           (org-end-of-meta-data t)
           (let ((start (point))
                 (end (save-excursion (outline-next-heading) (point))))
             (delete-region start end)
             (unless (string-empty-p content)
               (insert content "\n")))))
       (org-set-property "DIDA_TID" tid)
       (org-set-property "DIDA_BASE" hash)
       (save-buffer)))))

(defun dida--due-for-org (snapshot)
  "把 snapshot 的 due 转成 `org-schedule' 接受的字符串。"
  (let ((due (plist-get snapshot :due)))
    (if (plist-get snapshot :all-day)
        due
      (concat (substring due 0 10) " " (substring due 11 16)))))

(defun dida--org-set-base (tid hash)
  "只更新 TID 的 DIDA_BASE 属性。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (when-let ((pos (dida--org-find-tid tid)))
       (goto-char pos)
       (org-set-property "DIDA_BASE" hash)
       (save-buffer)))))

(defun dida--org-insert-task (pid name snapshot hash tid)
  "把远端任务插入项目 PID 下。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (goto-char (dida--org-ensure-project pid name))
     (org-end-of-subtree t t)
     (unless (bolp) (insert "\n"))
     (let ((pos (point)))
       (insert (format "** %s %s\n"
                       (if (= (or (plist-get snapshot :status) 0) 2) "DONE" "TODO")
                       (or (plist-get snapshot :title) "")))
       (goto-char pos)
       (let ((priority (plist-get snapshot :priority)))
         (when (memq priority '(5 3 1))
           (org-priority (pcase priority (5 ?A) (3 ?B) (1 ?C)))))
       (let ((due (plist-get snapshot :due)))
         (when (and (stringp due) (not (string-empty-p due)))
           (org-schedule nil (dida--due-for-org snapshot))))
       (org-set-property "DIDA_TID" tid)
       (org-set-property "DIDA_BASE" hash)
       (let ((content (or (plist-get snapshot :content) "")))
         (unless (string-empty-p content)
           (org-end-of-meta-data t)
           (insert content "\n"))))
     (save-buffer))))

(defun dida--org-remove-task (tid)
  "移除 TID，必要时归档。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (when-let ((pos (dida--org-find-tid tid)))
       (goto-char pos)
       (let ((text (buffer-substring-no-properties
                    (org-entry-beginning-position) (org-entry-end-position))))
         (when (and dida-archive-file
                    (stringp dida-archive-file)
                    (not (string-empty-p dida-archive-file)))
           (dida--archive-text text))
         (delete-region (org-entry-beginning-position)
                        (org-entry-end-position)))
       (save-buffer)))))

(defun dida--archive-text (text)
  "把 TEXT 追加到归档文件。"
  (let ((file (expand-file-name dida-archive-file)))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert text)
      (unless (bolp) (insert "\n"))
      (save-buffer))))

(defun dida--org-move-task (tid pid name)
  "把 TID 移到项目 PID 下。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (when-let ((pos (dida--org-find-tid tid)))
       (goto-char pos)
       (let ((text (buffer-substring-no-properties
                    (org-entry-beginning-position) (org-entry-end-position))))
         (delete-region (org-entry-beginning-position)
                        (org-entry-end-position))
         (goto-char (dida--org-ensure-project pid name))
         (org-end-of-subtree t t)
         (unless (bolp) (insert "\n"))
         (insert text)
         (unless (bolp) (insert "\n")))
       (save-buffer)))))

(defun dida--org-set-tid-base-at-marker (marker tid hash)
  "在 MARKER 处写入 TID 和 HASH。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (goto-char marker)
     (org-back-to-heading t)
     (org-set-property "DIDA_TID" tid)
     (org-set-property "DIDA_BASE" hash)
     (save-buffer))))

;;; --- 远端收集 ---------------------------------------------------------------

(defun dida--remote-collect ()
  "拉取所有项目及未完成任务。请求失败直接中止，避免误删除。"
  (let* ((projects (dida-api-list-projects))
         (by-tid nil)
         (names nil))
    (when (null projects)
      (user-error "远端没有返回任何项目，已中止同步"))
    (dolist (project projects)
      (let* ((pid (alist-get 'id project))
             (name (alist-get 'name project))
             (data (dida-api-get-project-data pid))
             (tasks (append (alist-get 'tasks data) nil)))
        (push (cons pid name) names)
        (dolist (task tasks)
          (let ((tid (alist-get 'id task)))
            (when (assoc tid by-tid)
              (user-error "远端任务 id 重复：%s" tid))
            (push (cons tid (list :pid pid
                                  :title (alist-get 'title task)
                                  :raw task
                                  :snapshot (dida--snapshot-from-api task pid)))
                  by-tid)))))
    (list :projects projects
          :names names
          :pids (mapcar (lambda (project) (alist-get 'id project)) projects)
          :by-tid by-tid)))

;;; --- 计划 -------------------------------------------------------------------

(defun dida--op (action &rest plist)
  "构造一个同步操作。"
  (cons action plist))

(defun dida--plan (local remote)
  "根据本地/远端/状态生成操作列表。"
  (let* ((local-tasks (plist-get local :tasks))
         (local-projects (plist-get local :projects))
         (remote-by-tid (plist-get remote :by-tid))
         (remote-pids (plist-get remote :pids))
         (local-by-tid nil)
         (ops nil))
    (dolist (task local-tasks)
      (when-let ((tid (plist-get task :tid)))
        (push (cons tid task) local-by-tid)))
    ;; 远端有、本地没有的项目：补 heading
    (dolist (project (plist-get remote :projects))
      (let ((pid (alist-get 'id project)))
        (unless (cl-find pid local-projects
                         :key (lambda (p) (plist-get p :pid)) :test #'equal)
          (push (dida--op 'ensure-project
                          :pid pid :name (alist-get 'name project))
                ops))))
    ;; 本地新建任务
    (dolist (task local-tasks)
      (when (plist-get task :new)
        (push (dida--op 'create-remote :task task) ops)))
    ;; 本地已有任务
    (dolist (task local-tasks)
      (unless (or (plist-get task :new)
                  (dida-state--pending-p (plist-get task :tid)))
        (let* ((tid (plist-get task :tid))
               (pid (plist-get task :pid))
               (ls (plist-get task :snapshot))
               (base-hash (plist-get task :base))
               (remote-rec (cdr (assoc tid remote-by-tid))))
          (cond
           ((not (member pid remote-pids))
            (push (dida--op 'orphan :tid tid :pid pid
                            :title (plist-get task :title))
                  ops))
           ((null remote-rec)
            (push (dida--op 'local-only
                            :task task :base-hash base-hash)
                  ops))
           (t
            (let ((rs (plist-get remote-rec :snapshot))
                  (rpid (plist-get remote-rec :pid)))
              (if (not (equal pid rpid))
                  (push (dida--op 'move-local
                                  :tid tid :pid rpid
                                  :name (cdr (assoc rpid (plist-get remote :names))))
                        ops)
                (push (dida--plan-both tid pid ls rs
                                       (dida-state--base tid) base-hash)
                      ops))))))))
    ;; 远端有、本地没有
    (dolist (pair remote-by-tid)
      (let ((tid (car pair))
            (rec (cdr pair)))
        (unless (or (assoc tid local-by-tid)
                    (dida-state--pending-p tid))
          (let ((base (dida-state--base tid))
                (tombstone (dida-state--tombstone tid))
                (rs (plist-get rec :snapshot)))
            (cond
             ((or base tombstone)
              (if (and base (not (dida--snapshot-equal-p rs base)))
                  (push (dida--op 'conflict
                                  :tid tid :pid (plist-get rec :pid)
                                  :reason "deleted-locally-remote-edited"
                                  :base base :local nil :remote rs)
                        ops)
                (push (dida--op 'delete-remote
                                :tid tid :pid (plist-get rec :pid))
                      ops)))
             (t
              (push (dida--op 'create-local
                              :tid tid :pid (plist-get rec :pid)
                              :name (cdr (assoc (plist-get rec :pid)
                                                (plist-get remote :names)))
                              :remote rs)
                    ops)))))))
    ;; 两边都没有但还有 base：清理
    (dolist (pair (plist-get dida--state :bases))
      (let ((tid (car pair)))
        (unless (or (assoc tid local-by-tid)
                    (assoc tid remote-by-tid))
          (push (dida--op 'forget :tid tid) ops))))
    (nreverse ops)))

(defun dida--plan-both (tid pid local remote base base-hash)
  "处理两边都存在的任务，返回一个操作。"
  (let* ((local-hash (dida--snapshot-hash local))
         (remote-hash (dida--snapshot-hash remote)))
    (cond
     ((equal local-hash remote-hash)
      (dida--op 'agree :tid tid :pid pid :remote remote
                :hash remote-hash))
     ((or (null base) (null base-hash))
      (dida--op 'conflict :tid tid :pid pid :reason "no-baseline"
                :base nil :local local :remote remote))
     (t
      (let* ((local-changed (not (equal local-hash base-hash)))
             (remote-changed (not (dida--snapshot-equal-p remote base)))
             (merge (dida--merge-snapshots base local remote))
             (conflicts (plist-get merge :conflicting-fields)))
        (cond
         ((and (not local-changed) remote-changed)
          (dida--op 'pull :tid tid :pid pid :remote remote :hash remote-hash
                    :title (plist-get remote :title)))
         ((and local-changed (not remote-changed))
          (dida--op 'push :tid tid :pid pid :target local
                    :title (plist-get local :title)))
         ((null conflicts)
          (let ((target (plist-get merge :snapshot)))
            (cond
             ((dida--snapshot-equal-p target remote)
              (dida--op 'pull :tid tid :pid pid :remote remote
                        :hash (dida--snapshot-hash remote)))
             ((= (or (plist-get target :status) 0) 2)
              (dida--op 'push :tid tid :pid pid :target target
                        :title (plist-get target :title)))
             ((dida--snapshot-equal-p target local)
              (dida--op 'push :tid tid :pid pid :target target
                        :title (plist-get target :title)))
             (t
              (dida--op 'push :tid tid :pid pid :target target
                        :title (plist-get target :title))))))
         (t
          (dida--op 'conflict :tid tid :pid pid :reason "field-conflict"
                    :base base :local local :remote remote))))))))


;;; --- 执行 -------------------------------------------------------------------

(defun dida--create-payload (pid snapshot)
  "由 SNAPSHOT 构造创建任务的 payload。"
  (dida-api-task-payload
   :title (plist-get snapshot :title)
   :project-id pid
   :content (or (plist-get snapshot :content) "")
   :priority (plist-get snapshot :priority)
   :is-all-day (if (plist-get snapshot :all-day) t :json-false)
   :due-date (dida--snapshot-due->api snapshot)
   :repeat-flag (plist-get snapshot :repeat)))

(defun dida--push-target (tid pid target)
  "把 TARGET 推到远端；target 状态 2 时会再调用 complete。"
  (let* ((remote (dida-api-get-task pid tid))
         (payload (and remote (dida-api-payload-from-remote remote))))
    (unless remote
      (error "远端任务 %s 已不存在，不能 update" tid))
    (setq payload (plist-put payload :title (plist-get target :title)))
    (setq payload (plist-put payload :content (or (plist-get target :content) "")))
    (setq payload (plist-put payload :priority (plist-get target :priority)))
    (setq payload (plist-put payload :isAllDay
                             (if (plist-get target :all-day) t :json-false)))
    (setq payload (plist-put payload :dueDate
                             (dida--snapshot-due->api target)))
    (setq payload (plist-put payload :repeatFlag (plist-get target :repeat)))
    (setq payload (plist-put payload :projectId pid))
    (setq payload (plist-put payload :status 0))
    (dida-api-update-task tid payload)
    (when (= (or (plist-get target :status) 0) 2)
      (unless (dida--complete-outbox tid pid)
        (error "完成任务 %s 失败" tid))
      (dida--org-remove-task tid)
      (dida-state--del-base tid)
      (dida-state--del-pending tid))
    (let ((fresh (dida-api-get-task pid tid)))
      (when (and fresh (/= (or (plist-get target :status) 0) 2))
        (dida--snapshot-from-api fresh pid)))))

(defun dida--complete-outbox (tid pid)
  "可靠完成 TID；失败时保留 pending 并返回 nil。"
  (dida-state--add-pending tid pid)
  (condition-case err
      (progn
        (dida-api-complete-task pid tid)
        (let ((fresh (dida-api-get-task pid tid)))
          (when (and fresh (/= (or (alist-get 'status fresh) 0) 2))
            (let ((payload (dida-api-payload-from-remote fresh)))
              (setq payload (plist-put payload :status 2))
              (dida-api-update-task tid payload))
            (setq fresh (dida-api-get-task pid tid))
            (when (and fresh (/= (or (alist-get 'status fresh) 0) 2))
              (error "完成任务后状态仍不是 2"))))
        (dida-state--del-pending tid)
        t)
    (error
     (message "dida: 完成任务 %s 失败：%s" tid (error-message-string err))
     nil)))

(defun dida--create-remote-from-snapshot (pid snapshot &optional old-tid)
  "创建远端任务；成功返回新 id。"
  (let ((task (dida-api-create-task (dida--create-payload pid snapshot))))
    (unless (and task (alist-get 'id task))
      (error "创建结果未知（没有返回 id）"))
    (let* ((new-tid (alist-get 'id task))
           (new-snapshot (dida--snapshot-from-api task pid))
           (hash (dida--snapshot-hash new-snapshot)))
      (when old-tid
        (dida--org-set-tid-base old-tid new-tid hash)
        (dida-state--del-base old-tid))
      (dida-state--set-base new-tid new-snapshot pid)
      (when (= (or (plist-get snapshot :status) 0) 2)
        (dida--complete-outbox new-tid pid)
        (dida--org-remove-task new-tid)
        (dida-state--del-base new-tid))
      new-tid)))

(defun dida--org-set-tid-base (old-tid new-tid hash)
  "把 OLD-TID 的 heading 改成 NEW-TID 并写入 HASH。"
  (with-current-buffer (dida--org-buffer)
    (org-with-wide-buffer
     (let ((pos (dida--org-find-tid old-tid)))
       (unless pos (error "找不到任务 %s" old-tid))
       (goto-char pos)
       (org-set-property "DIDA_TID" new-tid)
       (org-set-property "DIDA_BASE" hash)
       (save-buffer)))))

(defun dida--execute-op (op)
  "执行一个同步操作，返回一个符号用于统计。"
  (let ((action (car op))
        (args (cdr op)))
    (pcase action
      ('ensure-project
       (dida--org-ensure-project (plist-get args :pid) (plist-get args :name))
       'project)
      ('agree
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (snapshot (plist-get args :remote)))
         (dida--org-set-base tid (plist-get args :hash))
         (dida-state--set-base tid snapshot pid)
         'agree))
      ('pull
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (snapshot (plist-get args :remote)))
         (dida--org-apply-snapshot tid snapshot (plist-get args :hash))
         (dida-state--set-base tid snapshot pid)
         'pull))
      ('push
       (let* ((tid (plist-get args :tid))
              (pid (plist-get args :pid))
              (snapshot (dida--push-target tid pid
                                           (plist-get args :target))))
         (when snapshot
           (dida--org-apply-snapshot tid snapshot
                                     (dida--snapshot-hash snapshot))
           (dida-state--set-base tid snapshot pid))
         'push))
      ('complete
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid)))
         (if (dida--complete-outbox tid pid)
             (progn
               (dida--org-remove-task tid)
               (dida-state--del-base tid)
               (dida-state--del-pending tid)
               'complete)
           'complete-pending)))
      ('create-remote
       (let* ((task (plist-get args :task))
              (pid (plist-get task :pid))
              (snapshot (plist-get task :snapshot))
              (task-info (dida-api-create-task (dida--create-payload pid snapshot))))
         (unless (and task-info (alist-get 'id task-info))
           (error "创建结果未知（没有返回 id）"))
         (let* ((tid (alist-get 'id task-info))
                (new-snapshot (dida--snapshot-from-api task-info pid))
                (hash (dida--snapshot-hash new-snapshot)))
           (dida--org-set-tid-base-at-marker (plist-get task :marker) tid hash)
           (dida-state--set-base tid new-snapshot pid)
           (when (= (or (plist-get snapshot :status) 0) 2)
             (dida--complete-outbox tid pid)
             (dida--org-remove-task tid)
             (dida-state--del-base tid))
           'create)))
      ('create-local
       (dida--org-insert-task (plist-get args :pid) (plist-get args :name)
                              (plist-get args :remote)
                              (dida--snapshot-hash (plist-get args :remote))
                              (plist-get args :tid))
       (dida-state--set-base (plist-get args :tid) (plist-get args :remote)
                             (plist-get args :pid))
       'create)
      ('delete-remote
       (dida-api-delete-task (plist-get args :pid) (plist-get args :tid))
       (dida-state--del-base (plist-get args :tid))
       (dida-state--del-pending (plist-get args :tid))
       'delete)
      ('local-only
       (let* ((task (plist-get args :task))
              (tid (plist-get task :tid))
              (snapshot (plist-get task :snapshot))
              (base-hash (plist-get args :base-hash)))
         (cond
          ((= (or (plist-get snapshot :status) 0) 2)
           (dida--org-remove-task tid)
           (dida-state--del-base tid)
           'remove)
          ((null base-hash)
           (let ((state-base (dida-state--base tid)))
             (if (and state-base
                      (dida--snapshot-equal-p snapshot state-base))
                 (progn
                   (dida--org-remove-task tid)
                   (dida-state--set-tombstone tid snapshot)
                   (dida-state--del-base tid)
                   'remove)
               (dida-state--add-conflict tid (plist-get task :pid)
                                         "remote-missing-no-baseline"
                                         state-base snapshot nil
                                         (plist-get task :title))
               'conflict)))
          ((not (equal (dida--snapshot-hash snapshot) base-hash))
           (dida-state--add-conflict tid (plist-get task :pid)
                                     "remote-missing-local-edited"
                                     nil snapshot nil
                                     (plist-get task :title))
           'conflict)
          (t
           (dida--org-remove-task tid)
           (dida-state--set-tombstone tid snapshot)
           (dida-state--del-base tid)
           'remove))))
      ('move-local
       (dida--org-move-task (plist-get args :tid) (plist-get args :pid)
                            (plist-get args :name))
       'move)
      ('forget
       (dida-state--del-base (plist-get args :tid))
       (dida-state--del-pending (plist-get args :tid))
       'forget)
      ('orphan
       (message "dida: 项目 %s 在远端不存在，跳过任务 %s"
                (plist-get args :pid) (plist-get args :title))
       'orphan)
      ('conflict
       (dida-state--add-conflict (plist-get args :tid)
                                 (plist-get args :pid)
                                 (plist-get args :reason)
                                 (plist-get args :base)
                                 (plist-get args :local)
                                 (plist-get args :remote)
                                 (or (plist-get args :title)
                                     (plist-get (plist-get args :local) :title)
                                     (plist-get (plist-get args :remote) :title)))
       'conflict)
      (_ 'skip))))

(defun dida--direction-allows-p (action direction)
  "方向过滤：pull 只做下行，push 只做上行。"
  (pcase direction
    ('nil t)
    ('pull (memq action '(agree pull create-local local-only
                          ensure-project orphan conflict forget)))
    ('push (memq action '(agree push complete create-remote
                          delete-remote move-local orphan conflict forget)))
    (_ t)))

(defun dida--run-ops (ops direction)
  "执行 OPS，返回统计 plist。"
  (let ((counts nil))
    (dolist (op ops)
      (let ((action (car op)))
        (if (not (dida--direction-allows-p action direction))
            (setq counts (plist-put counts :deferred
                                    (1+ (or (plist-get counts :deferred) 0))))
          (condition-case err
              (let ((result (dida--execute-op op)))
                (setq counts (plist-put counts result
                                        (1+ (or (plist-get counts result) 0)))))
            (error
             (message "dida: 同步操作 %s 失败：%s" action
                      (error-message-string err))
             (setq counts (plist-put counts :error
                                     (1+ (or (plist-get counts :error) 0)))))))))
    counts))

(defun dida--process-pending ()
  "重试完成 outbox 中的任务；成功的移除本地 DONE。"
  (let ((pending (copy-sequence (plist-get dida--state :pending))))
    (dolist (pair pending)
      (let ((tid (car pair))
            (pid (cdr pair)))
        (if (not (with-current-buffer (dida--org-buffer)
                   (dida--org-find-tid tid)))
            (dida-state--del-pending tid)
          (let ((snapshot (with-current-buffer (dida--org-buffer)
                            (goto-char (dida--org-find-tid tid))
                            (dida--snapshot-from-org))))
            (if (/= (or (plist-get snapshot :status) 0) 2)
                (dida-state--del-pending tid)
              (when (dida--complete-outbox tid pid)
                (dida--org-remove-task tid)
                (dida-state--del-base tid)
                (dida-state--del-pending tid)))))))))

(defun dida-sync (&optional direction)
  "执行一次同步。DIRECTION 为 `pull' 时只拉取，为 `push' 时只推送。"
  (interactive)
  (unless (dida-auth--ensure)
    (user-error "未授权：请先运行 M-x dida-authorize"))
  (dida-state--load)
  (unless (eq direction 'pull)
    (dida--process-pending))
  (let* ((local (dida--org-collect))
         (remote (dida--remote-collect))
         (ops (dida--plan local remote))
         (counts (dida--run-ops ops direction)))
    (setq dida--state
          (plist-put dida--state :last-sync (format-time-string "%FT%T%z")))
    (dida-state--save)
    (message "dida 同步完成：%s"
             (mapconcat (lambda (pair)
                          (format "%s=%s" (car pair) (cdr pair)))
                        (cl-loop for (key value) on counts by #'cddr
                                 collect (cons key value))
                        " "))))

(defun dida-sync-preview ()
  "只显示同步计划，不写任何东西。"
  (interactive)
  (dida-state--load)
  (let* ((local (dida--org-collect))
         (remote (dida--remote-collect))
         (ops (dida--plan local remote)))
    (with-output-to-temp-buffer "*dida-sync-preview*"
      (dolist (op ops)
        (let ((args (cdr op)))
          (princ (format "%-14s %-10s %s\n"
                         (car op)
                         (or (plist-get args :pid) "")
                         (or (plist-get args :title)
                             (plist-get (plist-get args :task) :title)
                             (plist-get (plist-get args :local) :title)
                             (plist-get (plist-get args :remote) :title)
                             "")))))
      (princ (format "\n共 %d 个操作\n" (length ops))))))

;;; --- 触发 -------------------------------------------------------------------

(defun dida-schedule-sync (&optional seconds)
  "延迟 SECONDS 秒后执行一次同步。"
  (when dida--sync-timer
    (cancel-timer dida--sync-timer))
  (setq dida--sync-timer
        (run-at-time (or seconds dida-sync-delay) nil #'dida-sync)))

(defun dida-push (&optional _change-plist)
  "状态变化后调度同步（保持 `org-trigger-hook' 兼容）。"
  (interactive)
  (when (and (buffer-file-name)
             (stringp dida-sync-file)
             (not (string-empty-p dida-sync-file))
             (string= (file-truename (buffer-file-name))
                      (file-truename (expand-file-name dida-sync-file))))
    (dida-schedule-sync)))

(defun dida-async-push (&optional change-plist)
  "兼容旧名字；等价于 `dida-push'。"
  (dida-push change-plist))

(defun dida-fetch ()
  "只从远端拉取（不推送本地改动）。"
  (interactive)
  (dida-sync 'pull))

;;; --- 冲突审查 ---------------------------------------------------------------

(defun dida-review-conflicts ()
  "显示待解决的冲突。"
  (interactive)
  (dida-state--load)
  (let ((conflicts (plist-get dida--state :conflicts)))
    (with-current-buffer (get-buffer-create "*dida-conflicts*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert "dida 冲突\n\n")
        (if (null conflicts)
            (insert "没有待解决的冲突。\n")
          (dolist (conflict conflicts)
            (let* ((tid (plist-get conflict :tid))
                   (title (or (plist-get conflict :title) tid))
                   (reason (or (plist-get conflict :reason) "conflict")))
              (insert (format "任务：%s [%s]\n原因：%s\n" title tid reason))
              (insert "Org 版本：" (prin1-to-string (plist-get conflict :local)) "\n")
              (insert "TickTick 版本：" (prin1-to-string (plist-get conflict :remote)) "\n")
              (insert-text-button "用 Org" 'follow-link t
                                  'action (lambda (_) (dida-resolve-conflict tid 'org)))
              (insert "   ")
              (insert-text-button "用 TickTick" 'follow-link t
                                  'action (lambda (_) (dida-resolve-conflict tid 'ticktick)))
              (insert "   ")
              (insert-text-button "暂时保留" 'follow-link t
                                  'action (lambda (_) (message "已保留冲突")))
              (insert "\n\n")))))
      (pop-to-buffer (current-buffer)))))

(defun dida-resolve-conflict (tid choice)
  "解决 TID 的冲突。CHOICE 为 `org' 或 `ticktick'。"
  (dida-state--load)
  (let ((conflict (cl-find tid (plist-get dida--state :conflicts)
                           :key (lambda (c) (plist-get c :tid)) :test #'equal)))
    (unless conflict (user-error "冲突不存在"))
    (let* ((pid (plist-get conflict :pid))
           (local-snapshot (plist-get conflict :local))
           (remote (dida-api-get-task pid tid))
           (remote-snapshot (and remote (dida--snapshot-from-api remote pid)))
           (has-local (with-current-buffer (dida--org-buffer)
                        (dida--org-find-tid tid))))
      (pcase choice
        ('org
         (cond
          ((and remote has-local)
           (dida--push-target tid pid local-snapshot))
          (remote
           ;; 本地已删除：选择 Org 视为删除远端
           (dida-api-delete-task pid tid)
           (dida-state--del-base tid))
          (has-local
           (dida--create-remote-from-snapshot pid local-snapshot tid))
          (t (user-error "两边都没有可保留的版本"))))
        ('ticktick
         (cond
          ((and remote has-local)
           (dida--org-apply-snapshot tid remote-snapshot
                                     (dida--snapshot-hash remote-snapshot))
           (dida-state--set-base tid remote-snapshot pid))
          (remote
           (let ((name (alist-get 'name (dida-api-get-project pid))))
             (dida--org-insert-task pid name remote-snapshot
                                    (dida--snapshot-hash remote-snapshot)
                                    tid)
             (dida-state--set-base tid remote-snapshot pid)))
          (has-local
           (dida--org-remove-task tid)
           (dida-state--del-base tid))
          (t (user-error "两边都没有可保留的版本"))))
        (_ (user-error "未知选择")))
      (dida-state--remove-conflict tid)
      (dida-state--save)
      (message "冲突 %s 已解决" tid))))

(provide 'dida)
;;; dida.el ends here
