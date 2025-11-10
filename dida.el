(require 'plz)
(require 'async)
(require 's)

(defgroup dida nil
  "滴答清单"
  :group 'local)

(defcustom dida-client-id ""
  "注册dida365 API时系统生成的'client-id'"
  :type 'string
  :group 'dida)

(defcustom dida-client-secret ""
  "注册dida365 API时系统生成的'client-secret'"
  :type 'string
  :group 'dida)

(defcustom dida-auth-scopes "tasks:write tasks:read"
  "API的权限，这里默认是读写"
  :type 'string
  :group 'dida)

(defcustom dida-token-file (concat user-emacs-directory "didatoken")
  "存储dida token的文件路径，默认在'~/.emacs.d/didatoken'"
  :type 'file
  :group 'dida)

(defcustom dida-sync-file ""
  "dida清单同步到的一个文件路径，留空待用户自行设置"
  :type 'file
  :group 'dida)

(defcustom dida-token ""
  "根据dida API验证后获取到的token内容"
  :type 'string
  :group 'dida)

(defcustom dida-access-token ""
  "根据dida API验证后获取到的token内容中 'access_token'数值"
  :type 'string
  :group 'dida)

(defcustom dida-redirect-uri "http://localhost:1145"
  "注册dida365 API时填入的OAuth redirect url，需要在创建后进入app编辑界面设置"
  :type 'string
  :group 'dida)

(defvar dida-http-server nil
  "dida redirect HTTP 服务器进程")

;; 启动 HTTP 服务器用来显示Code
(defun dida-start-http-server (port)
  "Start HTTP server on specified PORT"
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

;; 服务器哨兵函数（处理状态变化）
(defun dida-server-sentinel (proc msg)
  ;; (message "Server sentinel: %s" msg)
  )

;; 解析 URL 查询参数
(defun dida-get-code-string (query)
  "获取 'code' 内容"
  (when (and query (string-match "code=\\(.\\{6\\}\\)&" query))
    (let ((dida-code (match-string 1 query)))
      (kill-new dida-code))))

;; 处理 HTTP 请求
(defun dida-http-server-filter (proc data)
  "Process incoming HTTP request"
  (when (string-match "\\([^ ]+\\) HTTP/1.[01]" data)
    (let* ((uri (match-string 1 data))
           (components (split-string uri "?"))
           (query (if (cdr components) (cadr components) nil))
           (code (dida-get-code-string query)))
      (let ((response-body (format "Code copied to kill-ring, please return to Emacs and yank\n" code)))
        (process-send-string 
         proc
         (format "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: %d\r\n\r\n%s"
                 (length response-body)
                 response-body)))
      ;; 关闭连接（简单实现中每次请求后关闭）
      (delete-process proc))))

;; 停止服务器
(defun dida-stop-http-server ()
  "Stop running HTTP server"
  (interactive)
  (when (process-live-p dida-http-server)
    (delete-process dida-http-server)
    (setq dida-http-server nil)))

(defun dida-auth ()
  "返回根据`dida-client-id' 和 `dida-client-secret' 生成的auth字符串"
  (concat
   "Basic "
   (base64-encode-string
    (concat dida-client-id ":" dida-client-secret) t)))

;;; Authorization functions (unchanged)
(defun dida-authorize ()
  "获取dida的验证token."
  (interactive)
  (let* ((state (format "%06x" (random (expt 16 6))))
         (auth-url (concat "https://dida365.com/oauth/authorize?"
                           (url-build-query-string
                            `(("client_id" ,dida-client-id)
                              ("response_type" "code")
                              ("redirect_uri" ,dida-redirect-uri)
                              ("scope" ,dida-auth-scopes)
                              ("state" ,state)))))
         code token-response)
    (browse-url auth-url)
    (dida-start-http-server 1145)
    (setq code (read-string "请粘贴code: "))
    (dida-stop-http-server)
    (setq dida-token (dida--exchange-code-for-token code))
    (if dida-token
        (progn
          (with-temp-file dida-token-file
            (prin1 dida-token (current-buffer)))
          (setq dida-access-token (plist-get dida-token :access_token))
          (message "验证成功！"))
      (message "获取token失败."))))

(defun dida--exchange-code-for-token (code)
  "把输入的'code'交换为'token'并返回."
  (let ((res (plz 'post "https://dida365.com/oauth/token"
               :headers `(("Authorization" . ,(dida-auth))
                          ("Content-Type" . "application/x-www-form-urlencoded"))
               :body (mapconcat
                      (lambda (kv)
                        (concat (car kv) "=" (cdr kv)))
                      `(("grant_type" . "authorization_code")
                        ("code" . ,code)
                        ("redirect_uri" . ,ticktick-redirect-uri)
                        ("scope" . ,ticktick-auth-scopes))
                      "&")
               :body-type 'text
               :as 'string)))
    (message "%s" res)
    (let ((json-object-type 'plist))
      (setq token-data (json-read-from-string res)))
    (if (plist-get token-data :access_token)
        (progn
          (plist-put token-data :created_at (float-time))
          token-data)
      nil)))

(defun dida--check-token ()
  "检查token是否过期，过期的话提醒一下"
  ;; 读取文件保存的token
  (with-temp-buffer
    (insert-file-contents dida-token-file)
    (setq dida-token (read (current-buffer))))
  (if dida-token
      (if (dida--token-expired-p dida-token)
          (message "token已过期，请重新获取！")
        (setq dida-access-token (plist-get dida-token :access_token)))
    (message "未能读取token，考虑重新获取！")))

(defun dida--token-expired-p (token)
  "计算'token'是否过期，默认nil，过期输出t."
  (let ((expires-in (plist-get token :expires_in))
        (created-at (plist-get token :created_at)))
    (if (and expires-in created-at)
        (> (float-time) (+ created-at expires-in -30))
      nil)))

;; 由于dida公开的API有限，这里直接打包为不同函数
;; 不过并没有用到全部函数
(defun dida-get-task-by-pid-tid (pid tid)
  "根据项目 ID 和任务 ID 获取任务，都是字符串"
  (append
   (plz 'get (concat "https://api.dida365.com/open/v1/project/" pid "/task/" tid)
     :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))
     :as #'json-read) nil))

;; (cl-defun dida-create-task (title pid &optional &key content org-time (priority 0) repeatflag)
;;   "创建任务"
;;   (let* ((timed (when org-time (plist-get (cadr org-time) ':raw-value)))
;;          (isallday (if (and timed (string-match "[0-9]:[0-9]" timed))
;;                        nil
;;                      t))
;;          (reminder (if isallday
;;                        '("TRIGGER:P0DT9H0M0S")
;;                      '("TRIGGER:PT0S")))
;;          (duedate (when timed
;;                     (format-time-string
;;                    (if isallday
;;                        "%Y-%m-%dT00:00:00+0800"
;;                      "%Y-%m-%dT%H:%M:00+0800")
;;                    (org-timestamp-to-time org-time)))))
;;     (plz 'post "https://api.dida365.com/open/v1/task"
;;       :headers `(("Authorization" . ,(concat "Bearer " dida-access-token))
;;                  ("Content-Type" . "application/json"))
;;       :body (json-encode `(:title ,title :projectId ,pid
;;                                   :content ,content
;;                                   :isAllDay ,isallday
;;                                   :dueDate ,duedate
;;                                   :priority ,priority
;;                                   :reminders ,reminder
;;                                   :repeatFlag ,repeatflag))
;;       :as #'json-read)))

(cl-defun dida-update-task (pid tid &optional &key title content org-time priority status repeatflag)
  "更新任务"
  (let* ((timed (plist-get (cadr org-time) ':raw-value))
         (isallday (if (and timed (string-match "[0-9]:[0-9]" timed))
                       nil
                     t))
         (reminder (if isallday
                       '("TRIGGER:P0DT9H0M0S")
                     '("TRIGGER:PT0S")))
         (duedate (when timed
                    (format-time-string
                   (if isallday
                       "%Y-%m-%dT00:00:00+0800"
                     "%Y-%m-%dT%H:%M:00+0800")
                   (org-timestamp-to-time org-time)))))
    (plz 'post (concat "https://api.dida365.com/open/v1/task/" tid )
      :headers `(("Authorization" . ,(concat "Bearer " dida-access-token))
                 ("Content-Type" . "application/json"))
      :body (json-encode `(:id ,tid :projectId ,pid
                               :title ,title
                               :content ,content
                               :isAllDay ,isallday
                               :dueDate ,duedate
                               :priority ,priority
                               :status ,status
                               :reminders ,reminder
                               :repeatFlag ,repeatflag)))))

(defun dida-complete-task (pid tid)
  "完成任务"
  (plz 'post (concat "https://api.dida365.com/open/v1/project/" pid "/task/" tid "/complete")
    :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))))

(defun dida-delete-task (pid tid)
  "删除任务"
  (plz 'delete (concat "https://api.dida365.com/open/v1/project/" pid "/task/" tid)
    :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))))

(defun dida-get-user-project ()
  "获取用户project"
  (append 
   (plz 'get "https://api.dida365.com/open/v1/project/"
     :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))
     :as #'json-read) nil))

(defun dida-get-project-by-id (pid)
  "根据'pid'获取用户project"
  (append
   (plz 'get (concat "https://api.dida365.com/open/v1/project/" pid)
     :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))
     :as #'json-read) nil))

(defun dida-get-project-by-id-with-data (pid)
  "根据'pid'获取用户project，带所有下属task的data"
  (append
   (plz 'get (concat "https://api.dida365.com/open/v1/project/" pid "/data")
     :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))
     :as #'json-read) nil))

;; (cl-defun dida-create-project (name &optional &key color (viewmode "list") (kind "TASK"))
;;   "创建用户project"
;;   (plz 'post "https://api.dida365.com/open/v1/project/"
;;     :headers `(("Authorization" . ,(concat "Bearer " dida-access-token))
;;                ("Content-Type" . "application/json"))
;;     :body (json-encode `(:name ,name :color ,color
;;                                :viewMode ,viewmode
;;                                :kind ,kind))))

(cl-defun dida-update-project (pid &optional &key name color (viewmode "list") (kind "TASK"))
  "更新用户project"
  (plz 'post (concat "https://api.dida365.com/open/v1/project/" pid)
    :headers `(("Authorization" . ,(concat "Bearer " dida-access-token))
               ("Content-Type" . "application/json"))
    :body (json-encode `(:name ,name :color ,color
                               :viewMode ,viewmode
                               :kind ,kind))))

(defun dida-delete-project (pid)
  "删除project"
  (plz 'delete (concat "https://api.dida365.com/open/v1/project/" pid)
    :headers `(("Authorization" . ,(concat "Bearer " dida-access-token)))))

;;;###autoload
(defun dida-fetch ()
  "从dida获取所有任务并覆盖本地."
  (interactive)
  (dida--check-token)
  (let ((projects (dida-get-user-project)))
    (with-current-buffer (find-file-noselect dida-sync-file)
       (erase-buffer)  
       (setq-local buffer-file-coding-system 'utf-8)
       (dolist (project projects)
         (let* ((project-id (alist-get 'id project))
                (project-name (alist-get 'name project))
                (project-data (dida-get-project-by-id-with-data project-id))
                (tasks (append (alist-get 'tasks project-data) nil)))
           ;; Insert project heading
           (insert (format "* %s\n:PROPERTIES:\n:DIDA_PID: %s\n:END:\n" 
                           project-name project-id))
           ;; Insert all tasks under this project
           (dolist (task tasks)
             (insert (dida--task-to-heading task)))))
       (save-buffer))))

(cl-defun dida--format-task-insert-string (&optional &key status priority title due-date isallday content id repeatflag)
  "将输入的参数拼接成一个可插入的task-string"
  (let* ((title title)
         (deadline-or-scheduled "SCHEDULED: ")
         (tid-or-did "DIDA_TID")
         (h-m (if (eq isallday ':json-false) " %H:%M" ""))
         (repeater (if repeatflag (downcase (replace-regexp-in-string "RRULE:FREQ=\\(.\\).+;INTERVAL=\\(.+\\)" " +\\2\\1" repeatflag)) "")))
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
                        (concat "<%Y-%m-%d %a"
                                h-m 
                                repeater
                                ">")
                        (date-to-time due-date))))
             "\n:PROPERTIES:\n:" tid-or-did ": " id "\n:END:\n")
            (when content
              (concat "\n" content "\n")))))

;; ;;;###autoload
;; (defun dida--update-stashed-tid-or-did ()
;;   "扫描本地文件，更新现存tid列表"
;;   (setq dida-stashed-tid nil)
;;   (with-current-buffer (find-file-noselect dida-sync-file)
;;     ;; (org-with-wide-buffer
;;      (goto-char (point-min))
;;      (while (outline-next-heading)
;;        (unless (org-entry-get nil "DIDA_PID")  ; Skip project headings
;;          (when (org-entry-get nil "DIDA_TID")
;;            (push (org-entry-get nil "DIDA_TID") dida-stashed-tid))
;;          (when (org-entry-get nil "DIDA_DID")
;;            (push (org-entry-get nil "DIDA_DID") dida-stashed-tid))))))

;;;###autoload
(defun dida--task-to-heading (task)
  "将一项dida task数据转换为org的heading字符串。根据观察，只会返回未完成的task。"
  (let* ((id (alist-get 'id task))
         (pid (alist-get 'projectId task))
         (isallday (alist-get 'isAllDay task))
         (title (alist-get 'title task ))
         (content (alist-get 'content task ))
         (status (alist-get 'status task))
         (due-date (alist-get 'dueDate task))
         (priority (alist-get 'priority task))
         (repeatflag (alist-get 'repeatFlag task))
         (insert-string (dida--format-task-insert-string :status status :priority priority :title title :due-date due-date :isallday isallday :content content :id id :repeatflag repeatflag)))
     insert-string))

;;;###autoload
(defun dida-push (change-plist)
  "分情况更新"
  (when (org-entry-get nil "DIDA_TID")
    (let ((from-state (format "%s" (plist-get change-plist :from)))
          (to-state (format "%s" (plist-get change-plist :to)))
          (tid (org-entry-get nil "DIDA_TID"))
          (pid (org-entry-get nil "DIDA_PID" t)))
      (if (string= to-state "DONE")
         (dida-complete-task pid tid) 
        (let* ((element (org-element-at-point))
               (title (org-element-property :title element))
               (todo-type (org-element-property :todo-type element))
               (priority (pcase (org-element-property :priority element)
                           (?A 5)
                           (?B 3)
                           (?C 1)
                           (_ 0)))
               (scheduled (org-element-property :scheduled element))
               (content (when (org-element-property :contents-begin element)
                          (string-trim (buffer-substring-no-properties
                                        (org-element-property :contents-begin element)
                                        (org-element-property :contents-end element)))))
               (repeatflag (when-let ((repeat-string (org-get-repeat)))
                             (concat "RRULE:FREQ="
                                     (pcase (substring repeat-string -1)
                                       (d "DAILY;")
                                       (w "WEEKLY;")
                                       (m "MONTHLY;")
                                       (y "YEARLY;"))
                                     "INTERVAL="
                                     (when (string-match "\\([0-9]+\\)" repeat-string)
                                       (match-string 1 repeat-string))))))
          (dida-update-task pid tid :title title :content content :org-time scheduled :priority priority :status 0 :repeatflag repeatflag))))))

(provide 'dida)
;;; dida.el ends here
