;;; test-dida-workflow.el --- dida-workflow.el 的离线测试 -*- lexical-binding: t; -*-

;; 用法（在本仓库根目录）：
;;   emacs -Q --batch -L . -L /path/to/plz -l test/test-dida-workflow.el
;;
;; 通过 dida-api-async-transport-function 注入一个内存里的假滴答服务
;; （假异步 transport 同步地调用 callback，所以「调用后即完成」）；
;; 覆盖：本地/远端新建、标题与日期双向、完成、放弃、删除、
;; 重复任务跳过、清单移动、空文件建清单、正文双向、冲突与解决、preview 只读、
;; 等待远端期间 dida.org 被改过时取消本轮。

(require 'dida)
(let ((root (expand-file-name ".."
                              (file-name-directory
                               (or load-file-name buffer-file-name)))))
  (load (expand-file-name "dida-workflow.el" root) nil t))

(setq dida-token "fake-token")
(setq dida-api-transport-function #'fake-transport)

(defvar fake-projects nil)
(defvar fake-tasks nil)
(defvar fake-counter 0)
(defvar fake-get-fail nil)
(defvar fake-include-completed nil)
(defvar fake-after-request-hook nil
  "假异步 transport 在每次请求完成后调用 (METHOD URL)。
用来模拟「等待网络期间用户改了 dida.org」等场景。")
(defvar fake-escape-brackets nil
  "非 nil 时模拟滴答编辑器：把正文方括号转义成 markdown 的 \\[ / \\]。
滴答会把文本节点里的方括号转义（免得被当成链接），Org 里的时间戳
[2026-11-19 四 10:53] 就是这么被写成 \\[...\\] 的。")

(defun fake--resp (status body)
  (make-plz-response :status status :headers nil :body body))

(defun fake--json-read (str)
  (let ((json-object-type 'alist)
        (json-key-type 'symbol)
        (json-array-type 'list)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string str)))

(defun fake--path (url)
  (car (split-string (url-filename (url-generic-parse-url url)) "?")))

(defun fake--all-tasks ()
  (let (out)
    (maphash (lambda (_pid tasks) (setq out (append out tasks))) fake-tasks)
    out))

(defun fake--find-task (tid)
  (cl-find-if (lambda (task) (equal (alist-get 'id task) tid)) (fake--all-tasks)))

(defun fake--project-by-id (pid)
  (cl-find-if (lambda (p) (equal (alist-get 'id p) pid)) fake-projects))

(defun fake--active-tasks (pid)
  (if fake-include-completed
      (gethash pid fake-tasks)
    (cl-remove-if (lambda (task) (= (or (alist-get 'status task) 0) 2))
                  (gethash pid fake-tasks))))

(defun fake--store-task (pid task)
  (puthash pid (append (gethash pid fake-tasks) (list task)) fake-tasks))

(defun fake--delete-task (pid tid)
  (puthash pid (cl-remove-if (lambda (task) (equal (alist-get 'id task) tid))
                             (gethash pid fake-tasks))
           fake-tasks))

(defun fake-transport (method url &rest args)
  (let* ((path (fake--path url))
         (body (plist-get args :body)))
    (cond
     ;; 项目列表
     ((and (equal method "GET") (string-match "^/open/v1/project/?$" path))
      (fake--resp 200 (json-encode (vconcat fake-projects))))
     ;; 项目数据
     ((and (equal method "GET")
           (string-match "^/open/v1/project/\\([^/]+\\)/data$" path))
      (let ((pid (match-string 1 path)))
        (fake--resp 200
                    (json-encode (list (cons 'project (fake--project-by-id pid))
                                       (cons 'tasks
                                             (vconcat (fake--active-tasks pid))))))))
     ;; 完成任务
     ((and (equal method "POST")
           (string-match "^/open/v1/project/\\([^/]+\\)/task/\\([^/]+\\)/complete$" path))
      (let* ((pid (match-string 1 path))
             (tid (match-string 2 path))
             (task (fake--find-task tid)))
        (if task
            (progn (setcdr (assoc 'status task) 2)
                   (fake--resp 200 ""))
          (fake--resp 404 "{}"))))
     ;; 移动任务
     ((and (equal method "POST") (string= path "/open/v1/task/move"))
      (let* ((payload (fake--json-read body))
             (item (car payload))
             (from (alist-get 'fromProjectId item))
             (to (alist-get 'toProjectId item))
             (tid (alist-get 'taskId item)))
        (if-let ((task (fake--find-task tid)))
            (progn
              (fake--delete-task from tid)
              (setcdr (assoc 'projectId task) to)
              (fake--store-task to task)
              (fake--resp 200 ""))
          (fake--resp 404 "{}"))))
     ;; 单任务 GET
     ((and (equal method "GET")
           (string-match "^/open/v1/project/\\([^/]+\\)/task/\\([^/]+\\)$" path))
      (if-let ((task (fake--find-task (match-string 2 path))))
          (fake--resp 200 (json-encode task))
        (fake--resp 404 "{}")))
     ;; 删除任务
     ((and (equal method "DELETE")
           (string-match "^/open/v1/project/\\([^/]+\\)/task/\\([^/]+\\)$" path))
      (fake--delete-task (match-string 1 path) (match-string 2 path))
      (fake--resp 200 ""))
     ;; 更新任务
     ((and (equal method "POST")
           (string-match "^/open/v1/task/\\([^/]+\\)$" path))
      (let* ((tid (match-string 1 path))
             (payload (fake--json-read body))
             (task (fake--find-task tid)))
        (if task
            (progn
              (dolist (pair payload)
                (let ((cell (assoc (car pair) task)))
                  (if cell (setcdr cell (cdr pair))
                    (push pair task))))
              (fake--maybe-escape-brackets task)
              (fake--resp 200 ""))
          (fake--resp 404 "{}"))))
     ;; 创建任务
     ((and (equal method "POST") (string= path "/open/v1/task"))
      (let* ((payload (fake--json-read body))
             (pid (alist-get 'projectId payload))
             (id (format "T%d" (cl-incf fake-counter)))
             (task (copy-tree payload)))
        (push (cons 'id id) task)
        (push (cons 'status 0) task)
        (push (cons 'tags []) task)
        (push (cons 'items []) task)
        (push (cons 'reminders []) task)
        (push (cons 'projectId pid) task)
        (fake--maybe-escape-brackets task)
        (fake--store-task pid task)
        (fake--resp 200 (json-encode task))))
     (t
      (fake--resp 404 "{}")))))

(defun fake-transport-async (method url callback &rest args)
  "假异步 transport：同步地调用 CALLBACK，让测试「调用后即完成」。
真实 plz 的回调是异步的（sentinel 里 run-at-time），但测试只需验证
结果与执行顺序，同步回调最稳定。"
  (condition-case err
      (let ((response (apply #'fake-transport method url args)))
        (when fake-after-request-hook
          (funcall fake-after-request-hook method url))
        (funcall callback response nil))
    (error
     (funcall callback
              nil
              (if (eq (car err) 'dida-api-error)
                  (cadr err)
                (dida-api--error-data
                 'client :message (error-message-string err)))))))

(setq dida-api-async-transport-function #'fake-transport-async)

(defun fake--maybe-escape-brackets (task)
  "模拟滴答编辑器：FAKE-ESCAPE-BRACKETS 非 nil 时转义 TASK 正文的方括号。

逐字符拼接，不用 `replace-regexp-in-string'：它的 replacement 会再解释一遍
反斜杠，想插一个反斜杠要写四层转义，容易写错。
这里就是滴答把文本节点里的 [ 和 ] 变成反斜杠转义形态的等价模拟。"
  (when fake-escape-brackets
    (let ((cell (assoc 'content task)))
      (when (and cell (stringp (cdr cell)))
        (setcdr cell
                (mapconcat (lambda (ch)
                             (if (memq ch '(?\[ ?\]))
                                 (concat "\\" (char-to-string ch))
                               (char-to-string ch)))
                           (string-to-list (cdr cell))
                           ""))))))

(defun fake-reset ()
  (setq fake-include-completed nil)
  (setq fake-escape-brackets nil)
  (setq fake-projects (list (list (cons 'id "P1") (cons 'name "Inbox"))
                            (list (cons 'id "P2") (cons 'name "生活")))
        fake-tasks (make-hash-table :test #'equal)
        fake-counter 0
        fake-get-fail nil)
  (puthash "P1" nil fake-tasks)
  (puthash "P2" nil fake-tasks))

(defun fake-add-task (pid id title &rest args)
  (let ((task (list (cons 'id id)
                    (cons 'projectId pid)
                    (cons 'title title)
                    (cons 'status 0)
                    (cons 'priority (or (plist-get args :priority) 0))
                    (cons 'content (or (plist-get args :content) ""))
                    (cons 'isAllDay (or (plist-get args :is-all-day) :json-false))
                    (cons 'repeatFlag (plist-get args :repeat))
                    (cons 'dueDate (plist-get args :due))
                    (cons 'tags [])
                    (cons 'items [])
                    (cons 'reminders []))))
    (fake--store-task pid task)))

(defvar test-dir nil)
(defvar test-file nil)
(defvar test-state nil)
(defvar test-log nil)

(defun test-setup ()
  (setq org-todo-keywords '((sequence "待办(t)" "|" "完成(d)" "放弃(x)")))
  (setq test-dir (make-temp-file "my-dida-test-" t)
        test-file (expand-file-name "dida.org" test-dir)
        test-state (expand-file-name "state.eld" test-dir))
  (with-temp-file test-file
    (insert "#+title: test\n\n"
            "* Inbox\n:PROPERTIES:\n:DIDA_PID: P1\n:DIDA_LIST: Inbox\n:END:\n\n"
            "* 生活\n:PROPERTIES:\n:DIDA_PID: P2\n:DIDA_LIST: 生活\n:END:\n"))
  (setq my-dida-file test-file
        my-dida-state-file test-state
        my-dida-whitelist nil
        my-dida-out-of-scope-policy 'delete
        my-dida-content-policy 'two-way
        my-dida-sync-repeat-p nil
        my-dida-archive-file nil
        my-dida--quiet t
        my-dida--busy nil)
  (fake-reset))

(defun test-file-text ()
  (with-temp-buffer
    (insert-file-contents test-file)
    (buffer-string)))

(defun test-sync ()
  (let ((my-dida--quiet t))
    (my-dida--plan-and-run)))

(defun test-collect-remote ()
  "同步地跑异步的远端采集（假 transport 同步回调）。"
  (let (result)
    (my-dida--collect-remote-async
     (lambda (remote err)
       (setq result (or remote (error "collect-remote 失败：%S" err)))))
    result))

(defun test-add-local (list-name title)
  (with-current-buffer (find-file-noselect test-file)
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote list-name)))
    (org-back-to-heading t)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert (format "** 待办 %s\n" title))
    (save-buffer)))

(defun test-task-heading (title)
  (with-current-buffer (find-file-noselect test-file)
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*\\* 待办 %s$" (regexp-quote title)) nil t)
      (org-back-to-heading t)
      (point))))

(defun test-set-title (old new)
  (with-current-buffer (find-file-noselect test-file)
    (goto-char (point-min))
    (re-search-forward (format "^\\*\\* \\(待办\\|完成\\|放弃\\) %s$" (regexp-quote old)))
    (org-back-to-heading t)
    (org-edit-headline new)
    (save-buffer)))

(defun test-mark-done (title)
  (with-current-buffer (find-file-noselect test-file)
    (goto-char (point-min))
    (re-search-forward (format "^\\*\\* 待办 %s$" (regexp-quote title)))
    (org-back-to-heading t)
    (org-todo "完成")
    (save-buffer)))

(defun test-get-task (tid)
  (fake--find-task tid))

(defmacro test-check (label form)
  `(if ,form
       (push (cons ,label t) test-log)
     (push (cons ,label nil) test-log)))

;;; ---------------- 测试 ----------------

(test-setup)

;; 1. 本地新任务 -> 远端
(test-add-local "Inbox" "本地新建")
(test-sync)
(let ((text (test-file-text)))
  (test-check 'local-create-remote
              (and (test-get-task "T1")
                   (equal (alist-get 'title (test-get-task "T1")) "本地新建")
                   (string-match ":DIDA_TID: T1" text))))

;; 2. 远端新任务 -> 本地
(fake-add-task "P1" "R1" "远端来的" :due "2026-11-06T00:00:00+0800" :is-all-day t)
(test-sync)
(let ((text (test-file-text)))
  (test-check 'remote-create-local
              (and (string-match "远端来的" text)
                   (string-match ":DIDA_TID: R1" text)
                   (string-match "SCHEDULED: <2026-11-06" text))))

;; 3. 本地改标题 -> 远端
(test-set-title "远端来的" "本地改标题")
(test-sync)
(test-check 'local-push-title
            (equal (alist-get 'title (test-get-task "R1")) "本地改标题"))

;; 4. 远端改日期 -> 本地
(let ((task (test-get-task "R1")))
  (setcdr (assoc 'dueDate task) "2026-11-09T00:00:00+0800")
  (setcdr (assoc 'isAllDay task) t))
(test-sync)
(let ((text (test-file-text)))
  (test-check 'remote-pull-date
              (string-match "SCHEDULED: <2026-11-09" text)))

;; 5. 本地完成 -> 远端完成 + 本地移除
(test-mark-done "本地改标题")
(test-sync)
(test-check 'local-complete
            (and (= (alist-get 'status (test-get-task "R1")) 2)
                 (not (string-match "本地改标题" (test-file-text)))))

;; 6. 远端新任务 -> 本地 -> 本地删除 -> 远端删除
(fake-add-task "P1" "R2" "待删任务")
(test-sync)
(test-check 'remote-create-2 (string-match "待删任务" (test-file-text)))
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 待删任务$")
  (org-back-to-heading t)
  (delete-region (org-entry-beginning-position) (org-entry-end-position))
  (save-buffer))
(test-sync)
(test-check 'out-of-scope-delete (null (test-get-task "R2")))

;; 7. 远端重复任务 -> 跳过
(fake-add-task "P1" "R3" "重复任务" :repeat "RRULE:FREQ=DAILY;INTERVAL=1")
(test-sync)
(test-check 'repeat-skip (not (string-match "重复任务" (test-file-text))))

;; 8. 清单移动
(fake-add-task "P1" "R4" "要换清单")
(test-sync)
(test-check 'move-setup (string-match "要换清单" (test-file-text)))
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 要换清单$")
  (org-back-to-heading t)
  (let ((text (buffer-substring-no-properties
               (org-entry-beginning-position) (org-entry-end-position))))
    (delete-region (org-entry-beginning-position) (org-entry-end-position))
    (goto-char (point-min))
    (re-search-forward "^\\* 生活$")
    (org-back-to-heading t)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert text "\n")
    (save-buffer)))
(test-sync)
(test-check 'move-remote
            (equal (alist-get 'projectId (test-get-task "R4")) "P2"))

;; 9. preview 不写文件
(let ((before (test-file-text))
      (before-count (length (fake--all-tasks))))
  (my-dida--plan-and-run t)
  (test-check 'preview-pure
              (and (equal before (test-file-text))
                   (= before-count (length (fake--all-tasks))))))


;; 10. 空文件自动建清单 heading
(test-setup)
(with-temp-file test-file (insert "#+title: empty\n"))
(fake-add-task "P2" "E1" "空文件来的")
(test-sync)
(test-check 'ensure-project
            (and (string-match "^\\* 生活$" (test-file-text) )
                 (string-match "空文件来的" (test-file-text))))

;; 11. 放弃 -> 远端完成 + 本地移除
(test-setup)
(fake-add-task "P1" "C1" "要放弃")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 要放弃$")
  (org-todo "放弃")
  (save-buffer))
(test-sync)
(test-check 'cancelled-complete
            (and (= (alist-get 'status (test-get-task "C1")) 2)
                 (not (string-match "要放弃" (test-file-text)))))

;; 12. out-of-scope detach：远端保留
(test-setup)
(setq my-dida-out-of-scope-policy 'detach)
(fake-add-task "P1" "D1" "要脱钩")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 要脱钩$")
  (org-back-to-heading t)
  (delete-region (org-entry-beginning-position) (org-entry-end-position))
  (save-buffer))
(test-sync)
(test-check 'out-of-scope-detach
            (and (test-get-task "D1")
                 (not (string-match "要脱钩" (test-file-text)))))

;; 13. 正文双向
(test-setup)
(fake-add-task "P1" "X1" "正文任务" :content "远端正文")
(test-sync)
(test-check 'content-insert (string-match "远端正文" (test-file-text)))
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (search-forward "远端正文")
  (replace-match "本地正文")
  (save-buffer))
(test-sync)
(test-check 'content-push
            (equal (alist-get 'content (test-get-task "X1")) "本地正文"))

;; 14. 连续同步不抖动
(test-setup)
(fake-add-task "P1" "N1" "稳定任务")
(test-sync)
(let ((text1 (test-file-text))
      (task1 (copy-tree (test-get-task "N1"))))
  (test-sync)
  (test-check 'no-churn
              (and (equal text1 (test-file-text))
                   (equal task1 (test-get-task "N1")))))

;; 15. 冲突 -> 用滴答解决
(test-setup)
(fake-add-task "P1" "K1" "冲突任务")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 冲突任务$")
  (org-back-to-heading t)
  (org-edit-headline "本地改")
  (save-buffer))
(setcdr (assoc 'title (test-get-task "K1")) "远端改")
(test-sync)
(test-check 'conflict-stored
            (and (plist-get (my-dida--task-state "K1") :conflict)
                 (string-match "本地改" (test-file-text))
                 (equal (alist-get 'title (test-get-task "K1")) "远端改")))
(my-dida--resolve "K1" 'remote)
(test-check 'conflict-resolve-remote
            (and (string-match "远端改" (test-file-text))
                 (null (plist-get (my-dida--task-state "K1") :conflict))))


;; 16. 即使 API 把已完成任务返回在 project/data 里，也不能插回本地
(test-setup)
(setq fake-include-completed t)
(fake-add-task "P1" "DONE1" "已完成任务")
(setcdr (assoc 'status (test-get-task "DONE1")) 2)
(test-sync)
(test-check 'completed-not-inserted
            (not (string-match "已完成任务" (test-file-text))))

;; 17. 本地任务在远端完成后：本地移除，且不会下一轮又出现
(test-setup)
(setq fake-include-completed t)
(fake-add-task "P1" "DONE2" "会完成的任务")
(test-sync)
(test-check 'completed-setup (string-match "会完成的任务" (test-file-text)))
(setcdr (assoc 'status (test-get-task "DONE2")) 2)
(test-sync)
(test-check 'completed-removed
            (not (string-match "会完成的任务" (test-file-text))))
(test-sync)
(test-check 'completed-not-reappear
            (not (string-match "会完成的任务" (test-file-text))))

;; 18. 本地标题里的标签、统计 cookie 原样推到滴答，且不来回抖动
(test-setup)
(test-add-local "Inbox" "买菜 [/] :errand:urgent:")
(test-sync)
(test-check 'local-title-verbatim
            (equal (alist-get 'title (test-get-task "T1"))
                   "买菜 [/] :errand:urgent:"))
(test-sync)
(test-check 'local-title-verbatim-no-churn
            (equal (alist-get 'title (test-get-task "T1"))
                   "买菜 [/] :errand:urgent:"))
(test-check 'local-heading-kept
            (string-match (regexp-quote "** 待办 买菜 [/] :errand:urgent:")
                          (test-file-text)))

;; 19. 远端标题里的标签、统计 cookie 拉回本地后保留，且下一轮不丢
(test-setup)
(fake-add-task "P1" "R1" "远端任务 [/] :tag_a:tag_b:")
(test-sync)
(test-check 'remote-title-verbatim
            (string-match (regexp-quote "** 待办 远端任务 [/] :tag_a:tag_b:")
                          (test-file-text)))
(test-sync)
(test-check 'remote-title-verbatim-no-strip
            (equal (alist-get 'title (test-get-task "R1"))
                   "远端任务 [/] :tag_a:tag_b:"))

;; 20. 本地改 Org 标签 -> 滴答标题里的标签文本跟着变
(test-setup)
(fake-add-task "P1" "R2" "任务甲 :old:")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 任务甲")
  (org-back-to-heading t)
  (org-set-tags '("new" "more"))
  (save-buffer))
(test-sync)
(test-check 'local-tag-edit-push
            (equal (alist-get 'title (test-get-task "R2"))
                   "任务甲 :new:more:"))

;; 21. 远端标题去掉标签 -> 本地 Org 标签也去掉
(test-setup)
(fake-add-task "P1" "R3" "任务乙 :old:")
(test-sync)
(test-check 'remote-tag-pulled (string-match ":old:" (test-file-text)))
(setcdr (assoc 'title (test-get-task "R3")) "任务乙")
(test-sync)
(test-check 'remote-tag-removed
            (not (string-match ":old:" (test-file-text))))

;; 22. 优先级 / DEADLINE 不推滴答，只推 SCHEDULED
(test-setup)
(test-add-local "Inbox" "任务丙")
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 任务丙$")
  (org-back-to-heading t)
  (org-priority ?A)
  (org-schedule nil "2026-11-22")
  (org-deadline nil "2026-11-20")
  (save-buffer))
(test-sync)
(let ((task (test-get-task "T1")))
  (test-check 'priority-not-pushed (null (alist-get 'priority task)))
  (test-check 'scheduled-pushed
              (equal (alist-get 'dueDate task) "2026-11-22T00:00:00+0800"))
  (test-check 'deadline-not-pushed
              (equal (alist-get 'isAllDay task) t)))
(test-check 'local-priority-deadline-kept
            (let ((text (test-file-text)))
              (and (string-match "\\[#A\\]" text)
                   (string-match "DEADLINE: <2026-11-20" text))))

;; 23. 远端优先级不拉回本地 Org
(test-setup)
(fake-add-task "P1" "R4" "任务丁" :priority 5)
(test-sync)
(test-check 'remote-priority-not-pulled
            (not (string-match "\\[#A\\]" (test-file-text))))

;; 24. 远端 dueDate 只写本地 SCHEDULED，不写 DEADLINE
(test-setup)
(fake-add-task "P1" "R5" "任务戊" :due "2026-11-23T09:30:00+0800")
(test-sync)
(let ((text (test-file-text)))
  (test-check 'remote-due-to-scheduled
              (and (string-match "SCHEDULED: <2026-11-23" text)
                   (string-match "09:30" text)))
  (test-check 'remote-due-not-deadline
              (not (string-match "DEADLINE:" text))))

;; 25. 本地改 SCHEDULED -> 推远端 dueDate
(test-setup)
(fake-add-task "P1" "R6" "任务己")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 任务己$")
  (org-back-to-heading t)
  (org-schedule nil "2026-11-24")
  (save-buffer))
(test-sync)
(test-check 'local-scheduled-pushed
            (equal (alist-get 'dueDate (test-get-task "R6"))
                   "2026-11-24T00:00:00+0800"))

;; 26. 清单 heading 已存在时不再每轮生成 ensure-project 空操作
(test-setup)
(fake-add-task "P1" "S1" "清单检查")
(test-sync)
(let* ((my-dida--quiet t)
       (local (my-dida--collect-local))
       (remote (test-collect-remote))
       (ops (my-dida--plan local remote)))
  (test-check 'no-ensure-project-when-exists
              (not (cl-some (lambda (op) (eq (car op) 'ensure-project)) ops))))

;; 27. 带标签 / 统计 cookie 的标题连续同步不抖动
(test-setup)
(test-add-local "Inbox" "买菜 [/] :errand:urgent:")
(test-sync)
(let* ((my-dida--quiet t)
       (local (my-dida--collect-local))
       (remote (test-collect-remote))
       (rec (car (plist-get local :tasks)))
       (snapshot (plist-get rec :snapshot))
       (ops (my-dida--plan local remote)))
  ;; 标题去掉文本属性，指纹才不会因 org-category/org-stats 抖动
  (test-check 'title-snapshot-no-text-properties
              (null (cl-loop for i from 0 below (length (plist-get snapshot :title))
                             thereis (text-properties-at i (plist-get snapshot :title)))))
  (test-check 'tag-title-no-churn-ops
              (not (cl-some (lambda (op) (memq (car op) '(push pull merge))) ops))))
(let ((text1 (test-file-text)))
  (test-sync)
  (test-check 'tag-title-no-churn-file (equal text1 (test-file-text))))

;; 28. 等待远端期间 dida.org 被用户改过 → 本轮取消，不覆盖用户编辑
(test-setup)
(test-add-local "Inbox" "保护本地修改")
(let ((fired nil))
  (setq fake-after-request-hook
        (lambda (_method _url)
          (unless fired
            (setq fired t)
            (with-current-buffer (find-file-noselect test-file)
              (goto-char (point-max))
              (insert "** 待办 用户刚加的\n")))))
  (test-sync)
  (setq fake-after-request-hook nil)
  (test-check 'abort-when-buffer-modified
              (and (null (test-get-task "T1"))
                   (with-current-buffer (find-file-noselect test-file)
                     (and (buffer-modified-p)
                          (string-match "用户刚加的" (buffer-string))))
                   (null my-dida--busy))))

;; 29. 手机/网页创建的全天任务：dueDate 可能被归一成 UTC，不能退一天
(test-setup)
(fake-add-task "P1" "U1" "UTC 全天任务"
               :due "2026-11-05T16:00:00+0000" :is-all-day t)
(test-sync)
(test-check 'all-day-utc-due-date
            (string-match "SCHEDULED: <2026-11-06" (test-file-text)))
(let ((text1 (test-file-text)))
  (test-sync)
  (test-check 'all-day-utc-no-churn (equal text1 (test-file-text))))

;; 30. 全天 dueDate 的其它归一形式：都要落在 11-06（按 dida-time-zone-offset 换算）
(test-setup)
(fake-add-task "P1" "U2" "UTC零点全天" :due "2026-11-06T00:00:00+0000" :is-all-day t)
(fake-add-task "P1" "U3" "本地零点全天" :due "2026-11-06T00:00:00+0800" :is-all-day t)
(test-sync)
(let ((text (test-file-text)))
  (test-check 'all-day-normalized-dates
              (and (= 2 (cl-count-if
                         (lambda (line) (string-match "SCHEDULED: <2026-11-06" line))
                         (split-string text "\n")))
                   (not (string-match "SCHEDULED: <2026-11-05" text)))))
(test-check 'api-due-normalization
            (equal (mapcar (lambda (due)
                             (my-dida--api-due
                              (list (cons 'dueDate due) (cons 'isAllDay t))))
                           '("2026-11-05T16:00:00+0000"
                             "2026-11-06T00:00:00+0000"
                             "2026-11-06T00:00:00+0800"))
                   '("2026-11-06" "2026-11-06" "2026-11-06")))

;; 31. 本地删除 SCHEDULED -> 远端 dueDate 清空，且后续同步不会把日期拉回来
(test-setup)
(fake-add-task "P1" "C1" "取消计划的时间" :due "2026-11-30T09:00:00+0800")
(test-sync)
(test-check 'cancel-scheduled-initial-pull
            (string-match "SCHEDULED: <2026-11-30" (test-file-text)))
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 取消计划的时间$")
  (org-back-to-heading t)
  ;; C-u C-c C-s：删除 SCHEDULED 时间
  (org-schedule '(4))
  (save-buffer))
(test-sync)
(test-check 'cancel-scheduled-cleared-remote
            (null (alist-get 'dueDate (test-get-task "C1"))))
(test-check 'cancel-scheduled-not-relocalized
            (not (string-match "SCHEDULED: <2026-11-30" (test-file-text))))
;; 再同步两轮，确认不会来回抖动
(test-sync)
(test-sync)
(test-check 'cancel-scheduled-no-churn
            (and (null (alist-get 'dueDate (test-get-task "C1")))
                 (not (string-match "SCHEDULED: <2026-11-30" (test-file-text)))))

;; 32. 本地删除全天 SCHEDULED -> 远端仍要清空（isAllDay 不能把日期留住）
(test-setup)
(fake-add-task "P1" "C2" "取消全天计划" :due "2026-12-01T00:00:00+0800" :is-all-day t)
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 取消全天计划$")
  (org-back-to-heading t)
  (org-schedule '(4))
  (save-buffer))
(test-sync)
(test-check 'cancel-all-day-cleared-remote
            (null (alist-get 'dueDate (test-get-task "C2"))))
(test-sync)
(test-check 'cancel-all-day-no-pull-back
            (not (string-match "SCHEDULED: <2026-12-01" (test-file-text))))

;; 33. 取消日期之后，远端又设了日期 -> 仍然要能拉回本地（pull 方向没被改坏）
(test-setup)
(fake-add-task "P1" "C3" "取消后再排期" :due "2026-12-10T09:00:00+0800")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 取消后再排期$")
  (org-back-to-heading t)
  (org-schedule '(4))
  (save-buffer))
(test-sync)
(test-check 'cancel-then-remote-reschedule-cleared
            (null (alist-get 'dueDate (test-get-task "C3"))))
(let ((task (test-get-task "C3")))
  (setcdr (assoc 'dueDate task) "2026-12-15T09:00:00+0800")
  (setcdr (assoc 'isAllDay task) :json-false))
(test-sync)
(test-check 'remote-reschedule-pulled-again
            (string-match "SCHEDULED: <2026-12-15" (test-file-text)))

;; 34. 本地取消日期 + 远端改标题：字段级合并后日期清空、标题用远端的
(test-setup)
(fake-add-task "P1" "C4" "原来的标题" :due "2026-12-20T09:00:00+0800")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 原来的标题$")
  (org-back-to-heading t)
  (org-schedule '(4))
  (save-buffer))
(let ((task (test-get-task "C4")))
  (setcdr (assoc 'title task) "远端改的标题"))
(test-sync)
(test-check 'cancel-and-remote-title-merged
            (and (null (alist-get 'dueDate (test-get-task "C4")))
                 (equal (alist-get 'title (test-get-task "C4")) "远端改的标题")
                 (string-match "远端改的标题" (test-file-text))
                 (not (string-match "SCHEDULED: <2026-12-20" (test-file-text)))))

;; 35. 滴答正文里的 markdown checkbox `[x]' 要写成 Org 认的 `[X]'
(test-setup)
(fake-add-task "P1" "K1" "检查项任务"
               :content (concat "- [x] 已完成项\n"
                                "- [ ] 未完成项\n"
                                "说明里的 [x] 不是 checkbox"))
(test-sync)
(let ((text (test-file-text)))
  (test-check 'checkbox-md-to-org
              ;; 默认 case-fold-search 为 t，检查「没有留下小写 x」要关掉它
              (let ((case-fold-search nil))
                (and (string-match "^- \\[X\\] 已完成项$" text)
                     (string-match "^- \\[ \\] 未完成项$" text)
                     (not (string-match "^- \\[x\\]" text))
                     ;; 列表项以外位置的 `[x]' 不是 checkbox，不能动
                     (string-match "说明里的 \\[x\\] 不是 checkbox" text)))))
(with-current-buffer (find-file-noselect test-file)
  ;; Org 自己把这行解析成「已勾选的 checkbox」——C-c C-c、[/] 统计、
  ;; HTML 导出都只看 item 的 :checkbox 属性（小写 [x] 会解析成 nil）
  (test-check 'checkbox-org-recognized
              (cl-some (lambda (item)
                         ;; 值是符号 on / off / trans，不是字符串
                         (eq (org-element-property :checkbox item) 'on))
                       (org-element-map (org-element-parse-buffer) 'item
                                        #'identity))))
;; 再同步不应抖动：本地已是大写，远端仍是小写，指纹必须相同
(let ((text1 (test-file-text))
      (content1 (alist-get 'content (test-get-task "K1"))))
  (test-sync)
  (test-check 'checkbox-no-churn
              (and (equal text1 (test-file-text))
                   (equal content1 (alist-get 'content (test-get-task "K1"))))))

;; 36. 本地正文写 `[X]' 时推给滴答的是 markdown 的 `[x]'，且不抖动
(test-setup)
(fake-add-task "P1" "K2" "本地勾选项")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 本地勾选项$")
  (org-back-to-heading t)
  (org-end-of-meta-data t)
  (insert "- [X] 本地已完成\n")
  (save-buffer))
(test-sync)
(test-check 'checkbox-org-to-md
            ;; 推给滴答的正文要是 markdown 的小写形态（同样要关掉大小写折叠）
            (let ((case-fold-search nil))
              (equal (alist-get 'content (test-get-task "K2"))
                     "- [x] 本地已完成")))
(let ((text1 (test-file-text))
      (content1 (alist-get 'content (test-get-task "K2"))))
  (test-sync)
  (test-check 'checkbox-push-no-churn
              (and (equal text1 (test-file-text))
                   (equal content1 (alist-get 'content (test-get-task "K2"))))))

;; 37. 旧状态文件里的 base 是小写 checkbox：升级后不能误判成假冲突
(test-setup)
(fake-add-task "P1" "K3" "升级兼容" :content "- [x] 旧正文\n- [ ] 待办")
(test-sync)
;; 手工把 state 里的 base 正文改回旧版本的小写形态，模拟升级前保存的状态
(let ((file test-state))
  (with-temp-file file
    (let ((state (with-temp-buffer
                   (insert-file-contents file)
                   (read (current-buffer)))))
      (dolist (entry (plist-get state :tasks))
        (let ((base (plist-get (cdr entry) :base)))
          (when base
            (plist-put base :content
                       (replace-regexp-in-string "\\[X\\] " "[x] "
                                                 (or (plist-get base :content) ""))))))
      (let ((print-length nil) (print-level nil))
        (prin1 state (current-buffer))))))
(let* ((my-dida--quiet t)
       (ops (progn (my-dida--state-load)
                   (my-dida--plan (my-dida--collect-local)
                                  (test-collect-remote)))))
  (test-check 'checkbox-stale-base-no-conflict
              (not (cl-some (lambda (op) (eq (car op) 'merge)) ops))))
(let ((text1 (test-file-text)))
  (test-sync)
  (test-check 'checkbox-stale-base-no-churn (equal text1 (test-file-text))))

;; 38. checkbox 转换的边界：只动行首列表项，且 `]' 后要有空白
(let ((case-fold-search nil)
      (input (concat "- [x] 无序\n"
                     "* [x] 星号\n"
                     "+ [x] 加号\n"
                     "1. [x] 有序点\n"
                     "1) [x] 有序括号\n"
                     "  - [x] 缩进\n"
                     "[x] 没有列表符号\n"
                     "正文里的 [x] 不是 checkbox\n"
                     "- [x](http://example.com) 链接文字不是 checkbox\n"
                     "- [X] 已经是 Org 写法\n"
                     "- [-] 部分勾选不动")))
  (test-check 'checkbox-converter-forms
              (equal (my-dida--content-md->org input)
                     (concat "- [X] 无序\n"
                             "* [X] 星号\n"
                             "+ [X] 加号\n"
                             "1. [X] 有序点\n"
                             "1) [X] 有序括号\n"
                             "  - [X] 缩进\n"
                             "[x] 没有列表符号\n"
                             "正文里的 [x] 不是 checkbox\n"
                             "- [x](http://example.com) 链接文字不是 checkbox\n"
                             "- [X] 已经是 Org 写法\n"
                             "- [-] 部分勾选不动")))
  ;; 反向转换对称：行首列表项的 checkbox 都还原成 markdown 的小写形态
  (test-check 'checkbox-converter-reverse
              (equal (my-dida--content-org->md (my-dida--content-md->org input))
                     (concat "- [x] 无序\n"
                             "* [x] 星号\n"
                             "+ [x] 加号\n"
                             "1. [x] 有序点\n"
                             "1) [x] 有序括号\n"
                             "  - [x] 缩进\n"
                             "[x] 没有列表符号\n"
                             "正文里的 [x] 不是 checkbox\n"
                             "- [x](http://example.com) 链接文字不是 checkbox\n"
                             "- [x] 已经是 Org 写法\n"
                             "- [-] 部分勾选不动"))))

;; 39. 滴答编辑器把正文里的方括号转义：拉回 Org 必须还原成字面方括号
(test-setup)
(setq fake-escape-brackets t)
(fake-add-task "P1" "E1" "时间戳任务"
               :content "\\[2026-11-19 四 10:53\\] 刘云书记：开会")
(test-sync)
(test-check 'unescape-timestamp-on-pull
            (let ((text (test-file-text))
                  (case-fold-search nil))
              (and (string-match "^\\[2026-11-19 四 10:53\\] 刘云书记" text)
                   (not (string-match "\\\\\\[" text)))))
;; 还原后 Org 能把它解析成时间戳（带反斜杠时只是普通文本）
(with-current-buffer (find-file-noselect test-file)
  (test-check 'timestamp-parsed-by-org
              (cl-some (lambda (ts)
                         (string-match "2026-11-19"
                                       (or (org-element-property :raw-value ts) "")))
                       (org-element-map (org-element-parse-buffer) 'timestamp
                                        #'identity))))
;; 远端仍带转义，连续同步也不应抖动
(let ((text1 (test-file-text))
      (content1 (alist-get 'content (test-get-task "E1"))))
  (test-sync)
  (test-check 'unescape-no-churn
              (and (equal text1 (test-file-text))
                   (equal content1 (alist-get 'content (test-get-task "E1"))))))
(setq fake-escape-brackets nil)

;; 40. 本地时间戳推给滴答（服务端转义）后再拉回，Org 里仍是合法时间戳
(test-setup)
(setq fake-escape-brackets t)
(fake-add-task "P1" "E2" "本地时间戳")
(test-sync)
(with-current-buffer (find-file-noselect test-file)
  (goto-char (point-min))
  (re-search-forward "^\\*\\* 待办 本地时间戳$")
  (org-back-to-heading t)
  (org-end-of-meta-data t)
  (insert "[2026-11-19 四 10:53] 本地记的\n")
  (save-buffer))
(test-sync)
(test-check 'push-sends-plain-brackets
            (let ((case-fold-search nil))
              (equal (alist-get 'content (test-get-task "E2"))
                     "\\[2026-11-19 四 10:53\\] 本地记的")))
(test-sync)
(test-check 'escaped-roundtrip-keeps-org-timestamp
            (let ((text (test-file-text))
                  (case-fold-search nil))
              (and (string-match "^\\[2026-11-19 四 10:53\\] 本地记的$" text)
                   (not (string-match "\\\\\\[" text)))))
(let ((text1 (test-file-text)))
  (test-sync)
  (test-check 'escaped-roundtrip-no-churn (equal text1 (test-file-text))))
(setq fake-escape-brackets nil)

;; 41. 普通文本里的方括号也还原，且与 checkbox 归一互不干扰
(test-setup)
(fake-add-task "P1" "E3" "混合内容"
               :content "- [x] 已完成\\[1\\]\n\\[备注\\] 说明")
(test-sync)
(test-check 'unescape-mixed-content
            (let ((text (test-file-text))
                  (case-fold-search nil))
              (and (string-match "^- \\[X\\] 已完成\\[1\\]$" text)
                   (string-match "^\\[备注\\] 说明$" text)
                   (not (string-match "\\\\\\[" text)))))

;; 42. 转义还原的边界：只去掉方括号前的反斜杠，其它标点不动
(let ((case-fold-search nil))
  (test-check 'unescape-forms
              (and (equal (my-dida--content-md-unescape "\\[a\\]") "[a]")
                   (equal (my-dida--content-md-unescape "正常 [无转义] 文本")
                          "正常 [无转义] 文本")
                   (equal (my-dida--content-md-unescape "\\*星号\\*")
                          "\\*星号\\*")
                   ;; 字面反斜杠 + 方括号：只去掉转义那一层
                   (equal (my-dida--content-md-unescape "\\\\[x\\]") "\\[x]"))))

;;; ---------------- 结果 ----------------
(princ "\n==== RESULTS ====\n")
(let ((fails 0))
  (dolist (item (reverse test-log))
    (princ (format "%s %s\n" (if (cdr item) "PASS" "FAIL") (car item)))
    (unless (cdr item) (cl-incf fails)))
  (princ (format "\nfailures: %d\n" fails))
  (when (> fails 0) (kill-emacs 1)))
