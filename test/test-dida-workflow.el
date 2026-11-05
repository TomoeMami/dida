;;; test-dida-workflow.el --- dida-workflow.el 的离线测试 -*- lexical-binding: t; -*-

;; 用法（在本仓库根目录）：
;;   emacs -Q --batch -L . -L /path/to/plz -l test/test-dida-workflow.el
;;
;; 通过 dida-api-transport-function 注入一个内存里的假滴答服务，
;; 覆盖：本地/远端新建、标题与日期双向、完成、放弃、删除、
;; 重复任务跳过、清单移动、空文件建清单、正文双向、冲突与解决、preview 只读。

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
        (fake--store-task pid task)
        (fake--resp 200 (json-encode task))))
     (t
      (fake--resp 404 "{}")))))

(defun fake-reset ()
  (setq fake-include-completed nil)
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
       (remote (my-dida--collect-remote))
       (ops (my-dida--plan local remote)))
  (test-check 'no-ensure-project-when-exists
              (not (cl-some (lambda (op) (eq (car op) 'ensure-project)) ops))))

;; 27. 带标签 / 统计 cookie 的标题连续同步不抖动
(test-setup)
(test-add-local "Inbox" "买菜 [/] :errand:urgent:")
(test-sync)
(let* ((my-dida--quiet t)
       (local (my-dida--collect-local))
       (remote (my-dida--collect-remote))
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

;;; ---------------- 结果 ----------------
(princ "\n==== RESULTS ====\n")
(let ((fails 0))
  (dolist (item (reverse test-log))
    (princ (format "%s %s\n" (if (cdr item) "PASS" "FAIL") (car item)))
    (unless (cdr item) (cl-incf fails)))
  (princ (format "\nfailures: %d\n" fails))
  (when (> fails 0) (kill-emacs 1)))
