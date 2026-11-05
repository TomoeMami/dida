;;; dida-workflow.el --- 在 dida.el API 之上实现「日常清单」双向同步 -*- lexical-binding: t; -*-

;; Author: 由 dida.el 用户维护的自定义工作流层
;; Version: 0.1
;; Keywords: org, convenience
;; Package-Requires: ((emacs "27.1") (dida "2.0"))
;;
;; 本文件随 dida 包一起分发（同一仓库、TomoELPA 的 dida 包会把它装到包目录），
;; 只使用 dida.el 的 API 层：
;;   认证：dida-auth--ensure / dida-token
;;   请求：dida-api--request（内部由 dida-api-* 封装）
;;   资源：dida-api-list-projects / dida-api-get-project-data /
;;         dida-api-get-task / dida-api-create-task / dida-api-update-task /
;;         dida-api-complete-task / dida-api-delete-task / dida-api-move-task /
;;         dida-api-task-payload / dida-api-payload-from-remote
;;
;; 工作流模型（详见 WORKFLOW-RESET.org）：
;;   - dida.org：一级清单 heading + 二级任务（任务为叶子），与滴答双向；
;;   - 状态只有 待办 / 完成 / 放弃；完成与放弃在滴答侧都算完成；
;;   - 滴答里的重复任务跳过（重复任务留给 habit.org）；
;;   - 任务从 dida.org 剪走时，按 `my-dida-out-of-scope-policy' 处理远端任务；
;;   - 项目文件完全不参与同步。
;;
;; 用法：
;;   (require 'dida)          ; 先按 README 配好 client / token
;;   (require 'dida-workflow) ; 本文件放到 load-path
;;   (setq my-dida-file "~/denote/org/phone/dida.org")
;;   (my-dida-auto-sync-mode 1)
;;
;; 命令：
;;   M-x my-dida-sync           完整双向同步
;;   M-x my-dida-preview        只打印计划，不写任何东西
;;   M-x my-dida-push-heading   立即推送当前 heading（手动兜底）
;;   M-x my-dida-detach         去掉当前 heading 的 DIDA_* 属性与本地状态
;;   M-x my-dida-set-list       把当前任务换到另一个清单（同步时推 move）
;;   M-x my-dida-review         查看冲突/待确认删除
;;
;;; Code:

(require 'dida)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)

;;;; ---------------------------------------------------------------------------
;;;; 1. 配置
;;;; ---------------------------------------------------------------------------

(defgroup my-dida nil
  "基于 dida.el 的自定义日常清单同步。"
  :group 'dida)

(defcustom my-dida-file (expand-file-name "dida.org" "~/denote/org/phone/")
  "与滴答同步的日常区 org 文件。"
  :type 'file)

(defcustom my-dida-state-file (expand-file-name "my-dida-state.eld" "~/emacs-local/")
  "本机状态文件（base 快照 / pending / 冲突）。不要放进 Syncthing 目录。"
  :type 'file)

(defcustom my-dida-whitelist nil
  "只同步这些清单（清单名或 id）；nil 表示同步所有清单。"
  :type '(choice (const nil) (repeat string)))

(defcustom my-dida-todo-keyword "待办"
  "日常区里表示「待办」的 TODO 关键字。"
  :type 'string)

(defcustom my-dida-done-keyword "完成"
  "日常区里表示「完成」的 TODO 关键字。"
  :type 'string)

(defcustom my-dida-cancelled-keyword "放弃"
  "日常区里表示「放弃」的 TODO 关键字；推滴答时同样走 complete。"
  :type 'string)

(defcustom my-dida-out-of-scope-policy 'delete
  "任务从 dida.org 消失时如何处理滴答任务。
delete：删除滴答任务；detach：保留滴答任务、只清本地状态；
review：放进 my-dida-review 等你决定。"
  :type '(choice (const delete) (const detach) (const review)))

(defcustom my-dida-content-policy 'two-way
  "正文同步策略。
two-way：正文双向；pull-new：只在新任务落地时写入正文、之后不覆盖；
off：完全不读不写正文。"
  :type '(choice (const two-way) (const pull-new) (const off)))

(defcustom my-dida-sync-repeat-p nil
  "是否同步重复任务。默认 nil：远端 repeatFlag 非空的任务整条跳过。"
  :type 'boolean)

(defcustom my-dida-archive-file nil
  "非 nil 时，任务从本地移除前把条目文本追加到这个文件。"
  :type '(choice (const nil) file))

(defcustom my-dida-bulk-threshold 10
  "一轮同步里「从 dida.org 消失」的任务超过这个数时冻结，只报告不执行。"
  :type 'integer)

(defcustom my-dida-debounce 8
  "保存/状态变化后延迟多少秒触发同步。"
  :type 'integer)

(defcustom my-dida-idle-interval 300
  "空闲多少秒跑一次完整同步。"
  :type 'integer)

(defcustom my-dida-primary-device-p t
  "当前设备是否负责自动同步；nil 时自动模式只做手动/预览。
可以写成函数，例如 (lambda () (member (system-name) '(\"aaa-PC\")))。"
  :type '(choice boolean function))

(defcustom my-dida-auto-sync-startup-delay 20
  "启动后延迟多少秒做第一次同步。"
  :type 'integer)

(defvar my-dida--state nil "当前状态 plist。")
(defvar my-dida--busy nil "同步进行中标志。")
(defvar my-dida--quiet nil "非 nil 时不打印常规消息。")
(defvar my-dida--debounce-timer nil)
(defvar my-dida--idle-timer nil)
(defvar my-dida--remote-names nil "本轮同步的 pid → 清单名映射。")

;;;; ---------------------------------------------------------------------------
;;;; 2. 状态文件
;;;; ---------------------------------------------------------------------------

(defun my-dida--state-path ()
  (expand-file-name my-dida-state-file))

(defun my-dida--state-empty ()
  (list :version 1 :last-sync nil :tasks nil))

(defun my-dida--state-load ()
  "载入状态文件到 `my-dida--state'。"
  (setq my-dida--state (my-dida--state-empty))
  (when (file-exists-p (my-dida--state-path))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents (my-dida--state-path))
          (let ((data (read (current-buffer))))
            (when (and (listp data) (integerp (plist-get data :version)))
              (setq my-dida--state data))))
      (error
       (message "[my-dida] 状态文件损坏，已忽略：%s" (error-message-string err)))))
  my-dida--state)

(defun my-dida--state-save ()
  "原子写状态文件。"
  (let* ((path (my-dida--state-path))
         (dir (file-name-directory path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (let ((tmp (make-temp-file (expand-file-name ".my-dida-" dir))))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (let ((print-length nil) (print-level nil))
                (prin1 my-dida--state (current-buffer))))
            (ignore-errors (set-file-modes tmp #o600))
            (rename-file tmp path t))
        (when (file-exists-p tmp) (delete-file tmp))))))

(defun my-dida--task-state (tid)
  "返回 TID 的状态 plist。"
  (cdr (assoc tid (plist-get my-dida--state :tasks))))

(defun my-dida--task-state-set (tid plist)
  "设置 TID 的状态 plist。"
  (let ((tasks (assoc-delete-all tid (plist-get my-dida--state :tasks))))
    (push (cons tid plist) tasks)
    (setq my-dida--state
          (plist-put my-dida--state :tasks tasks))))

(defun my-dida--task-state-del (tid)
  "删除 TID 的状态。"
  (setq my-dida--state
        (plist-put my-dida--state :tasks
                   (assoc-delete-all tid (plist-get my-dida--state :tasks)))))

(defun my-dida--task-base (tid)
  (plist-get (my-dida--task-state tid) :base))

(defun my-dida--task-pid (tid)
  (plist-get (my-dida--task-state tid) :pid))

;;;; ---------------------------------------------------------------------------
;;;; 3. 快照与规范化
;;;; ---------------------------------------------------------------------------

(defconst my-dida--snapshot-fields
  '(:title :content :status :priority :due :all-day))

(defun my-dida--content-p ()
  "正文是否参与双向比较。"
  (eq my-dida-content-policy 'two-way))

(defun my-dida--content-push-p ()
  "推远端时是否覆盖正文。"
  (eq my-dida-content-policy 'two-way))

(defun my-dida--norm-text (s)
  (string-trim (or s "")))

(defun my-dida--norm-content (s)
  (let* ((s (or s ""))
         (s (replace-regexp-in-string "\r\n?" "\n" s)))
    (string-trim s)))

(defun my-dida--snapshot-hash (snapshot)
  "SNAPSHOT 的稳定指纹（不含 :cancelled）。"
  (secure-hash
   'sha1
   (prin1-to-string
    (mapcar (lambda (field) (cons field (plist-get snapshot field)))
            my-dida--snapshot-fields))))

(defun my-dida--snapshot-equal-p (a b)
  (and a b (equal (my-dida--snapshot-hash a) (my-dida--snapshot-hash b))))

(defun my-dida--org-snapshot ()
  "当前 heading 的快照。"
  (let* ((element (org-element-at-point))
         (done (org-entry-is-done-p))
         (priority (pcase (org-element-property :priority element)
                     (?A 5) (?B 3) (?C 1) (_ 0)))
         (status (if done 2 0))
         (cancelled (and (= status 2)
                         (equal (org-get-todo-state) my-dida-cancelled-keyword))))
    (list :title (my-dida--norm-text (org-element-property :title element))
          :content (if (my-dida--content-p)
                       (my-dida--norm-content (my-dida--direct-body))
                     "")
          :status status
          :priority priority
          :due (my-dida--org-due)
          :all-day (my-dida--org-all-day-p)
          :cancelled cancelled)))

(defun my-dida--org-due ()
  "当前 heading 的 SCHEDULED（否则 DEADLINE），返回 YYYY-MM-DD[THH:MM]。"
  (let* ((element (org-element-at-point))
         (ts (or (org-element-property :scheduled element)
                 (org-element-property :deadline element))))
    (when ts
      (let ((time (org-timestamp-to-time ts)))
        (if (org-element-property :hour-start ts)
            (format-time-string "%Y-%m-%dT%H:%M" time)
          (format-time-string "%Y-%m-%d" time))))))

(defun my-dida--org-all-day-p ()
  (let* ((element (org-element-at-point))
         (ts (or (org-element-property :scheduled element)
                 (org-element-property :deadline element))))
    (and ts (not (org-element-property :hour-start ts)))))

(defun my-dida--api-due (task)
  "远端 TASK 的 dueDate 归一成 YYYY-MM-DD[THH:MM]。"
  (let ((due (alist-get 'dueDate task))
        (all-day (eq (alist-get 'isAllDay task) t)))
    (when (and (stringp due) (not (string-empty-p due)))
      (if all-day
          (substring due 0 10)
        (format-time-string "%Y-%m-%dT%H:%M" (date-to-time due))))))

(defun my-dida--due->api (snapshot)
  "把 SNAPSHOT 的 due 转成 API 的 dueDate。"
  (let ((due (plist-get snapshot :due)))
    (when (and (stringp due) (not (string-empty-p due)))
      (if (plist-get snapshot :all-day)
          (concat due "T00:00:00" dida-time-zone-offset)
        (concat due ":00" dida-time-zone-offset)))))

(defun my-dida--due-for-org (snapshot)
  "把 SNAPSHOT 的 due 转成 `org-schedule' 接受的字符串。"
  (let ((due (plist-get snapshot :due)))
    (if (plist-get snapshot :all-day)
        due
      (concat (substring due 0 10) " " (substring due 11 16)))))

(defun my-dida--api-snapshot (task)
  "远端 TASK 的快照。"
  (let* ((status (alist-get 'status task))
         (all-day (eq (alist-get 'isAllDay task) t)))
    (list :title (my-dida--norm-text (alist-get 'title task))
          :content (if (my-dida--content-p)
                       (my-dida--norm-content (alist-get 'content task))
                     "")
          :status (pcase status (2 2) (-1 2) (_ 0))
          :priority (or (alist-get 'priority task) 0)
          :due (my-dida--api-due task)
          :all-day all-day
          :cancelled (eq status -1))))

(defun my-dida--merge-snapshots (base local remote)
  "三方合并，返回 (MERGED . CONFLICTS)。"
  (let (merged conflicts)
    (dolist (field my-dida--snapshot-fields)
      (let ((before (plist-get base field))
            (ours (plist-get local field))
            (theirs (plist-get remote field)))
        (cond
         ((equal ours theirs) (setq merged (plist-put merged field ours)))
         ((equal ours before) (setq merged (plist-put merged field theirs)))
         ((equal theirs before) (setq merged (plist-put merged field ours)))
         (t (push (list :field field :local ours :remote theirs) conflicts)
            (setq merged (plist-put merged field ours))))))
    (setq merged (plist-put merged :cancelled (plist-get remote :cancelled)))
    (cons merged (nreverse conflicts))))

;;;; ---------------------------------------------------------------------------
;;;; 4. Org 读写
;;;; ---------------------------------------------------------------------------

(defun my-dida--direct-body ()
  "当前 heading 的直属正文（不含子 heading）。"
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let* ((start (point))
           (limit (save-excursion (org-end-of-subtree t t) (point)))
           (end (save-excursion
                  (goto-char start)
                  (if (re-search-forward "^\\*+ " limit t)
                      (match-beginning 0)
                    limit))))
      (my-dida--norm-content
       (buffer-substring-no-properties start end)))))

(defun my-dida--has-children-p ()
  "当前 heading 是否有子 heading。"
  (save-excursion
    (org-back-to-heading t)
    (org-goto-first-child)))

(defun my-dida--find-tid (tid)
  "在当前 buffer 中按 DIDA_TID 查找位置。"
  (org-with-wide-buffer
   (catch 'found
     (org-map-entries
      (lambda ()
        (when (equal (org-entry-get nil "DIDA_TID") tid)
          (throw 'found (point))))
      nil nil)
     nil)))

(defun my-dida--find-project (pid)
  "在当前 buffer 中查找清单 heading（一级 + DIDA_PID）。"
  (org-with-wide-buffer
   (catch 'found
     (org-map-entries
      (lambda ()
        (when (and (= (org-current-level) 1)
                   (equal (org-entry-get nil "DIDA_PID") pid))
          (throw 'found (point))))
      nil nil)
     nil)))

(defun my-dida--set-base-property (tid hash)
  "在 TID 的 heading 上写 DIDA_BASE。"
  (when-let ((pos (my-dida--find-tid tid)))
    (goto-char pos)
    (org-set-property "DIDA_BASE" hash)))

(defun my-dida--apply-snapshot (tid snapshot)
  "把 SNAPSHOT 应用到 TID 的 heading。"
  (let ((pos (my-dida--find-tid tid)))
    (unless pos (error "找不到任务 %s" tid))
    (goto-char pos)
    (org-with-wide-buffer
     ;; 标题
     (let ((title (or (plist-get snapshot :title) "")))
       (unless (equal title (or (org-element-property :title (org-element-at-point)) ""))
         (org-edit-headline title)))
     ;; 状态：本地已经是 done 就保留 完成/放弃 的区别
     (let ((status (or (plist-get snapshot :status) 0)))
       (cond
        ((= status 2)
         (unless (org-entry-is-done-p)
           (org-todo (if (plist-get snapshot :cancelled)
                         my-dida-cancelled-keyword
                       my-dida-done-keyword))))
        ((org-entry-is-done-p) nil)
        (t
         ;; org-edit-headline 会连同 TODO 关键字一起重写；这里补回 待办
         (unless (equal (org-get-todo-state) my-dida-todo-keyword)
           (org-todo my-dida-todo-keyword)))))
     ;; 优先级
     (let ((priority (or (plist-get snapshot :priority) 0)))
       (cond
        ((memq priority '(5 3 1))
         (org-priority (pcase priority (5 ?A) (3 ?B) (1 ?C))))
        ((org-element-property :priority (org-element-at-point))
         (org-priority ?\s))))
     ;; 日期
     (if (plist-get snapshot :due)
         (org-schedule nil (my-dida--due-for-org snapshot))
       (when (org-entry-get nil "SCHEDULED")
         (org-schedule '(4))))
     ;; 正文
     (my-dida--apply-content snapshot)
     ;; 属性
     (org-set-property "DIDA_TID" tid)
     (org-set-property "DIDA_BASE" (my-dida--snapshot-hash snapshot)))))

(defun my-dida--apply-content (snapshot)
  "按正文策略把 SNAPSHOT 的 :content 写入直属正文。"
  (unless (eq my-dida-content-policy 'off)
    (let ((content (or (plist-get snapshot :content) "")))
      (when (or (my-dida--content-p)
                (and (eq my-dida-content-policy 'pull-new)
                     (string-empty-p (my-dida--direct-body))))
        (save-excursion
          (org-back-to-heading t)
          (org-end-of-meta-data t)
          (let* ((start (point))
                 (limit (save-excursion (org-end-of-subtree t t) (point)))
                 (end (save-excursion
                        (goto-char start)
                        (if (re-search-forward "^\\*+ " limit t)
                            (match-beginning 0)
                          limit))))
            (delete-region start end)
            (unless (string-empty-p content)
              (insert content "\n"))))))))

(defun my-dida--ensure-project (pid name)
  "确保 PID 对应的一级清单 heading 存在。"
  (unless (my-dida--find-project pid)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* %s\n:PROPERTIES:\n:DIDA_PID: %s\n:DIDA_LIST: %s\n:END:\n"
                    (or name pid) pid (or name "")))
    t))

(defun my-dida--insert-task (pid name tid snapshot raw-content)
  "把远端任务插入清单 PID 下。"
  (my-dida--ensure-project pid name)
  (goto-char (my-dida--find-project pid))
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  (let ((keyword (if (plist-get snapshot :cancelled)
                     my-dida-cancelled-keyword
                   (if (= (or (plist-get snapshot :status) 0) 2)
                       my-dida-done-keyword
                     my-dida-todo-keyword))))
    (insert (format "** %s %s\n" keyword (or (plist-get snapshot :title) ""))))
  ;; 回到刚插入的 heading
  (forward-line -1)
  (beginning-of-line)
  (org-set-property "DIDA_TID" tid)
  (org-set-property "DIDA_BASE" (my-dida--snapshot-hash snapshot))
  (let ((priority (or (plist-get snapshot :priority) 0)))
    (when (memq priority '(5 3 1))
      (org-priority (pcase priority (5 ?A) (3 ?B) (1 ?C)))))
  (when (plist-get snapshot :due)
    (org-schedule nil (my-dida--due-for-org snapshot)))
  (unless (eq my-dida-content-policy 'off)
    (when (and raw-content (not (string-empty-p raw-content)))
      (org-end-of-meta-data t)
      (insert (my-dida--norm-content raw-content) "\n"))))

(defun my-dida--remove-local (tid)
  "移除 TID 的 heading（先归档）。"
  (let ((pos (my-dida--find-tid tid)))
    (when pos
      (goto-char pos)
      (let ((text (buffer-substring-no-properties
                   (org-entry-beginning-position) (org-entry-end-position))))
        (my-dida--archive text))
      (delete-region (org-entry-beginning-position) (org-entry-end-position)))))

(defun my-dida--archive (text)
  (when (and my-dida-archive-file
             (stringp my-dida-archive-file)
             (not (string-empty-p my-dida-archive-file)))
    (let ((file (expand-file-name my-dida-archive-file)))
      (make-directory (file-name-directory file) t)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert text)
        (unless (bolp) (insert "\n"))
        (save-buffer)))))

(defun my-dida--move-local-task (tid pid name)
  "把本地 TID 的 heading 挪到清单 PID 下。"
  (let ((pos (my-dida--find-tid tid)))
    (unless pos (error "找不到任务 %s" tid))
    (goto-char pos)
    (let ((text (buffer-substring-no-properties
                 (org-entry-beginning-position) (org-entry-end-position))))
      (delete-region (org-entry-beginning-position) (org-entry-end-position))
      (my-dida--ensure-project pid name)
      (goto-char (my-dida--find-project pid))
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert text)
      (unless (bolp) (insert "\n")))))

;;;; ---------------------------------------------------------------------------
;;;; 5. 采集
;;;; ---------------------------------------------------------------------------

(defun my-dida--file ()
  (expand-file-name my-dida-file))

(defun my-dida--buffer ()
  (unless (file-exists-p (my-dida--file))
    (user-error "[my-dida] 找不到文件：%s" (my-dida--file)))
  (find-file-noselect (my-dida--file)))

(defun my-dida--project-selected-p (pid name)
  (or (null my-dida-whitelist)
      (member name my-dida-whitelist)
      (member pid my-dida-whitelist)))

(defun my-dida--collect-local ()
  "扫描 dida.org，返回 (:projects :tasks :new :buffer)。"
  (with-current-buffer (my-dida--buffer)
    (when (buffer-modified-p)
      (user-error "[my-dida] dida 文件有未保存修改，本轮跳过"))
    (org-with-wide-buffer
     (let (projects tasks news seen)
       (org-map-entries
        (lambda ()
          (let* ((level (org-current-level))
                 (pid (org-entry-get nil "DIDA_PID"))
                 (pid-inh (org-entry-get nil "DIDA_PID" t))
                 (tid (org-entry-get nil "DIDA_TID"))
                 (title (my-dida--norm-text
                         (org-element-property :title (org-element-at-point)))))
            (cond
             ((and pid (= level 1))
              (push (list :pid pid :name title) projects))
             (tid
              (when (member tid seen)
                (user-error "[my-dida] dida.org 中 DIDA_TID 重复：%s" tid))
              (push tid seen)
              (let ((snapshot (my-dida--org-snapshot)))
                (push (list :tid tid
                            :pid pid-inh
                            :title title
                            :marker (copy-marker (point))
                            :snapshot snapshot
                            :hash (my-dida--snapshot-hash snapshot)
                            :base-hash (org-entry-get nil "DIDA_BASE")
                            :invalid (and (my-dida--has-children-p) t)
                            :repeat (and (org-get-repeat) t))
                      tasks)))
             ((and (= level 2)
                   pid-inh
                   (org-get-todo-state)
                   (not (org-entry-get nil "DIDA_OFF"))
                   (not (org-get-repeat)))
              (let ((snapshot (my-dida--org-snapshot)))
                (push (list :new t
                            :pid pid-inh
                            :title title
                            :marker (copy-marker (point))
                            :snapshot snapshot
                            :raw-content (my-dida--direct-body))
                      news))))))
        nil nil)
       (list :buffer (current-buffer)
             :projects (nreverse projects)
             :tasks (nreverse tasks)
             :new (nreverse news))))))

(defun my-dida--repeat-task-p (task)
  (let ((repeat (alist-get 'repeatFlag task)))
    (and (stringp repeat) (not (string-empty-p repeat)))))

(defun my-dida--collect-remote ()
  "拉取远端清单与未完成任务。

`project/{pid}/data' 正常只返回未完成任务；这里仍显式忽略 status 2/-1，
避免服务端行为变化时把已完成任务当成「远端新任务」重新插回 dida.org。"
  (let* ((projects (dida-api-list-projects))
         (by-tid nil)
         (names nil)
         (repeats nil)
         (completed nil)
         (seen nil))
    (when (null projects)
      (user-error "[my-dida] 远端没有返回任何清单，已中止"))
    (dolist (project projects)
      (let ((pid (alist-get 'id project))
            (name (alist-get 'name project)))
        (when (my-dida--project-selected-p pid name)
          (push (cons pid name) names)
          (let* ((data (dida-api-get-project-data pid))
                 (tasks (append (alist-get 'tasks data) nil)))
            (dolist (task tasks)
              (let ((tid (alist-get 'id task)))
                (when (member tid seen)
                  (user-error "[my-dida] 远端任务 id 重复：%s" tid))
                (push tid seen)
                (let ((repeat (and (not my-dida-sync-repeat-p)
                                   (my-dida--repeat-task-p task)))
                      (status (alist-get 'status task)))
                  (cond
                   ;; 已完成 / 已放弃：绝不放进活跃集合
                   ((memq status '(2 -1))
                    (push (list :pid pid :name name
                                :title (alist-get 'title task))
                          completed))
                   ;; 重复任务：跳过
                   (repeat
                    (push (list :pid pid :name name
                                :title (alist-get 'title task))
                          repeats))
                   (t
                    (push (cons tid (list :pid pid
                                          :name name
                                          :task task
                                          :repeat nil
                                          :raw-content (or (alist-get 'content task) "")
                                          :snapshot (my-dida--api-snapshot task)))
                          by-tid))))))))))
    (setq my-dida--remote-names names)
    (list :names names
          :by-tid by-tid
          :repeats (nreverse repeats)
          :completed (nreverse completed))))

;;;; ---------------------------------------------------------------------------
;;;; 6. 计划
;;;; ---------------------------------------------------------------------------

(defun my-dida--base-hash-for (tid fallback)
  "TID 的 base 指纹；没有 state base 时用 FALLBACK（Org 属性里的指纹）。"
  (let ((base (my-dida--task-base tid)))
    (or (and base (my-dida--snapshot-hash base)) fallback)))

(defun my-dida--op (action &rest plist)
  (cons action plist))

(defun my-dida--plan-one (rec remote)
  "为一个本地已托管任务生成操作列表。"
  (let* ((tid (plist-get rec :tid))
         (pid (plist-get rec :pid))
         (snapshot (plist-get rec :snapshot))
         (ts (my-dida--task-state tid))
         (remote-rec (cdr (assoc tid (plist-get remote :by-tid)))))
    (cond
     ((plist-get rec :invalid)
      (list (my-dida--op 'skip :reason "任务下有子 heading，请剪到项目文件"
                         :title (plist-get rec :title))))
     ((plist-get rec :repeat)
      (list (my-dida--op 'skip :reason "日常区不允许重复任务，请挪到 habit.org"
                         :title (plist-get rec :title))))
     ((plist-get ts :pending)
      (if (= (or (plist-get snapshot :status) 0) 2)
          (list (my-dida--op 'pending-complete :tid tid :pid pid
                             :title (plist-get rec :title)))
        (list (my-dida--op 'clear-pending :tid tid
                           :title (plist-get rec :title)))))
     ((plist-get ts :conflict)
      (list (my-dida--op 'report-conflict :tid tid :title (plist-get rec :title))))
     ((plist-get remote-rec :repeat)
      (list (my-dida--op 'skip :reason "远端是重复任务，已跳过（重复任务请在 habit.org 管理）"
                         :title (plist-get rec :title))))
     ((= (or (plist-get snapshot :status) 0) 2)
      (list (my-dida--op 'local-done :tid tid :pid pid
                         :title (plist-get rec :title))))
     ((null remote-rec)
      (my-dida--plan-remote-missing rec))
     (t
      (my-dida--plan-both rec ts remote-rec remote)))))

(defun my-dida--plan-remote-missing (rec)
  "本地有、远端未完成列表里没有。"
  (let* ((tid (plist-get rec :tid))
         (base-hash (my-dida--base-hash-for tid (plist-get rec :base-hash))))
    (if (and base-hash (not (equal (plist-get rec :hash) base-hash)))
        (list (my-dida--op 'conflict :tid tid
                           :pid (plist-get rec :pid)
                           :reason "remote-missing-local-edited"
                           :title (plist-get rec :title)))
      (list (my-dida--op 'remote-gone :tid tid :pid (plist-get rec :pid)
                         :title (plist-get rec :title))))))

(defun my-dida--plan-move (rec ts remote-rec remote)
  "本地清单与远端清单不一致。"
  (let* ((tid (plist-get rec :tid))
         (pid (plist-get rec :pid))
         (rpid (plist-get remote-rec :pid))
         (base-pid (plist-get ts :pid)))
    (cond
     ((and base-pid (not (equal pid base-pid)) (equal rpid base-pid))
      (list (my-dida--op 'push-move :tid tid :pid pid :from rpid
                         :title (plist-get rec :title))))
     ((and base-pid (equal pid base-pid) (not (equal rpid base-pid)))
      (list (my-dida--op 'move-local :tid tid :pid rpid
                         :name (cdr (assoc rpid (plist-get remote :names)))
                         :title (plist-get rec :title))))
     (t
      (list (my-dida--op 'conflict :tid tid :pid pid :reason "list-conflict"
                         :title (plist-get rec :title)))))))

(defun my-dida--plan-both (rec ts remote-rec remote)
  "两边都存在且清单一致。"
  (let* ((tid (plist-get rec :tid))
         (pid (plist-get rec :pid))
         (snapshot (plist-get rec :snapshot))
         (rs (plist-get remote-rec :snapshot))
         (rpid (plist-get remote-rec :pid))
         (base (plist-get ts :base))
         (base-hash (my-dida--base-hash-for tid (plist-get rec :base-hash)))
         (local-hash (plist-get rec :hash))
         (remote-hash (my-dida--snapshot-hash rs)))
    (cond
     ;; 远端已完成（正常不会出现在 project/data，兜底）
     ((= (or (plist-get rs :status) 0) 2)
      (if (and base-hash (not (equal local-hash base-hash)))
          (list (my-dida--op 'conflict :tid tid :pid pid
                             :reason "remote-done-local-edited"
                             :title (plist-get rec :title)))
        (list (my-dida--op 'remote-gone :tid tid :pid pid
                           :title (plist-get rec :title)))))
     ;; 清单不一致
     ((not (equal pid rpid))
      (my-dida--plan-move rec ts remote-rec remote))
     ;; 完全一致
     ((equal local-hash remote-hash)
      (list (my-dida--op 'agree :tid tid :pid rpid :remote rs
                         :title (plist-get rec :title))))
     ;; 没有基线
     ((null base-hash)
      (list (my-dida--op 'conflict :tid tid :pid pid :reason "no-baseline"
                         :title (plist-get rec :title))))
     ;; 只有本地改
     ((equal remote-hash base-hash)
      (list (my-dida--op 'push :tid tid :pid pid :local snapshot
                         :title (plist-get rec :title))))
     ;; 只有远端改
     ((equal local-hash base-hash)
      (list (my-dida--op 'pull :tid tid :pid rpid :remote rs
                         :title (plist-get rec :title))))
     ;; 两边都改：字段级合并
     (t
      (my-dida--plan-merge tid pid snapshot rs base rec)))))

(defun my-dida--plan-merge (tid pid local remote base rec)
  "字段级合并 LOCAL/REMOTE。"
  (let ((merged (my-dida--merge-snapshots base local remote)))
    (if (cdr merged)
        (list (my-dida--op 'conflict :tid tid :pid pid :reason "field-conflict"
                           :local local :remote remote :base base
                           :title (plist-get rec :title)))
      (let ((target (car merged)))
        (cond
         ((my-dida--snapshot-equal-p target remote)
          (list (my-dida--op 'pull :tid tid :pid pid :remote remote
                             :target target :title (plist-get rec :title))))
         ((my-dida--snapshot-equal-p target local)
          (list (my-dida--op 'push :tid tid :pid pid :local local
                             :title (plist-get rec :title))))
         (t
          (list (my-dida--op 'merge :tid tid :pid pid :target target
                             :title (plist-get rec :title)))))))))

(defun my-dida--plan (local remote)
  "生成操作列表。"
  (let* ((local-tasks (plist-get local :tasks))
         (local-by-tid (mapcar (lambda (rec) (cons (plist-get rec :tid) rec))
                               local-tasks))
         (remote-by-tid (plist-get remote :by-tid))
         (local-projects (plist-get local :projects))
         (ops nil))
    ;; 1. 清单 heading
    (dolist (pair (plist-get remote :names))
      (unless (assoc (car pair) local-projects)
        (push (my-dida--op 'ensure-project :pid (car pair) :name (cdr pair))
              ops)))
    ;; 2. 本地已托管任务
    (dolist (rec local-tasks)
      (setq ops (append (nreverse (my-dida--plan-one rec remote)) ops)))
    ;; 3. 本地新任务
    (dolist (rec (plist-get local :new))
      (push (my-dida--op 'create-remote :rec rec
                         :title (plist-get rec :title)) ops))
    ;; 4. 远端新任务
    (dolist (pair remote-by-tid)
      (unless (or (assoc (car pair) local-by-tid)
                  (my-dida--task-state (car pair))
                  (plist-get (cdr pair) :repeat))
        (let ((rec (cdr pair)))
          (push (my-dida--op 'create-local :tid (car pair)
                             :pid (plist-get rec :pid)
                             :name (plist-get rec :name)
                             :remote (plist-get rec :snapshot)
                             :raw-content (plist-get rec :raw-content)
                             :title (plist-get (plist-get rec :snapshot) :title))
                ops))))
    ;; 5. 本地消失的任务
    (let ((missing nil))
      (dolist (pair (plist-get my-dida--state :tasks))
        (let* ((tid (car pair))
               (ts (cdr pair))
               (remote-rec (cdr (assoc tid remote-by-tid))))
          (unless (or (assoc tid local-by-tid)
                      (plist-get ts :pending)
                      (plist-get ts :conflict))
            (if remote-rec
                (push (my-dida--op 'out-of-scope :tid tid
                                   :pid (plist-get remote-rec :pid)
                                   :title (plist-get (plist-get remote-rec :snapshot)
                                                     :title))
                      missing)
              (push (my-dida--op 'forget :tid tid) ops)))))
      (if (> (length missing) my-dida-bulk-threshold)
          (push (my-dida--op 'bulk-freeze :count (length missing)) ops)
        (setq ops (append (nreverse missing) ops))))
    (nreverse ops)))

;;;; ---------------------------------------------------------------------------
;;;; 7. 执行
;;;; ---------------------------------------------------------------------------

(defun my-dida--complete-remote (tid pid)
  "可靠完成任务，返回 t/nil。"
  (condition-case err
      (progn
        (dida-api-complete-task pid tid)
        (let ((fresh (dida-api-get-task pid tid)))
          (cond
           ((null fresh) t)
           ((= (or (alist-get 'status fresh) 0) 2) t)
           (t
            (let ((payload (dida-api-payload-from-remote fresh)))
              (setq payload (plist-put payload :status 2))
              (dida-api-update-task tid payload))
            (let ((again (dida-api-get-task pid tid)))
              (or (null again)
                  (= (or (alist-get 'status again) 0) 2)))))))
    (error
     (my-dida--log "完成任务 %s 失败：%s" tid (error-message-string err))
     nil)))

(defun my-dida--push-snapshot (tid pid snapshot)
  "把 SNAPSHOT 推到远端 TID。"
  (let* ((remote (dida-api-get-task pid tid)))
    (unless remote (error "远端任务 %s 不存在" tid))
    (let ((payload (dida-api-payload-from-remote remote)))
      (setq payload (plist-put payload :title (or (plist-get snapshot :title) "")))
      (when (my-dida--content-push-p)
        (setq payload (plist-put payload :content
                                 (or (plist-get snapshot :content) ""))))
      (setq payload (plist-put payload :priority
                               (or (plist-get snapshot :priority) 0)))
      (setq payload (plist-put payload :status 0))
      (if (plist-get snapshot :due)
          (progn
            (setq payload (plist-put payload :dueDate (my-dida--due->api snapshot)))
            (setq payload (plist-put payload :isAllDay
                                     (if (plist-get snapshot :all-day)
                                         t :json-false))))
        (cl-remf payload :dueDate)
        (cl-remf payload :isAllDay))
      (dida-api-update-task tid payload))))

(defun my-dida--create-payload (pid snapshot content)
  (let ((due (my-dida--due->api snapshot)))
    (dida-api-task-payload
     :title (or (plist-get snapshot :title) "")
     :project-id pid
     :content (if (eq my-dida-content-policy 'off) "" content)
     :priority (or (plist-get snapshot :priority) 0)
     :due-date due
     :is-all-day (and due (if (plist-get snapshot :all-day) t :json-false)))))

(defun my-dida--create-remote (rec)
  "把本地新任务创建到滴答，返回新 TID。"
  (let* ((pid (plist-get rec :pid))
         (snapshot (plist-get rec :snapshot))
         (payload (my-dida--create-payload pid snapshot
                                           (plist-get rec :raw-content)))
         (task (dida-api-create-task payload)))
    (unless (and task (alist-get 'id task))
      (error "创建结果未知"))
    (let* ((tid (alist-get 'id task))
           (remote-snapshot (my-dida--api-snapshot task))
           (hash (my-dida--snapshot-hash remote-snapshot))
           (marker (plist-get rec :marker)))
      (with-current-buffer (my-dida--buffer)
        (goto-char marker)
        (org-back-to-heading t)
        (org-set-property "DIDA_TID" tid)
        (org-set-property "DIDA_BASE" hash))
      (my-dida--task-state-set tid (list :pid (or (alist-get 'projectId task) pid)
                                         :base remote-snapshot))
      tid)))

(defun my-dida--set-state (tid pid snapshot)
  (my-dida--task-state-set tid (list :pid pid :base snapshot))
  (with-current-buffer (my-dida--buffer)
    (my-dida--set-base-property tid (my-dida--snapshot-hash snapshot))))

(defun my-dida--exec (op)
  "执行一个操作，返回符号。"
  (let ((action (car op))
        (args (cdr op)))
    (pcase action
      ('ensure-project
       (with-current-buffer (my-dida--buffer)
         (my-dida--ensure-project (plist-get args :pid) (plist-get args :name)))
       'project)
      ('agree
       (my-dida--set-state (plist-get args :tid) (plist-get args :pid)
                           (plist-get args :remote))
       'agree)
      ('pull
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (remote (plist-get args :remote)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--apply-snapshot tid remote))
         (my-dida--set-state tid pid remote)
         'pull))
      ('push
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (local (plist-get args :local)))
         (my-dida--push-snapshot tid pid local)
         (my-dida--set-state tid pid local)
         'push))
      ('merge
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (target (plist-get args :target)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--apply-snapshot tid target))
         (my-dida--push-snapshot tid pid target)
         (my-dida--set-state tid pid target)
         'merge))
      ('local-done
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid)))
         (if (my-dida--complete-remote tid pid)
             (progn
               (with-current-buffer (my-dida--buffer)
                 (my-dida--remove-local tid))
               (my-dida--task-state-del tid)
               'complete)
           (my-dida--task-state-set tid
                                    (plist-put (my-dida--task-state tid) :pending t))
           'complete-pending)))
      ('pending-complete
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid)))
         (if (my-dida--complete-remote tid pid)
             (progn
               (with-current-buffer (my-dida--buffer)
                 (my-dida--remove-local tid))
               (my-dida--task-state-del tid)
               'complete)
           'complete-pending)))
      ('clear-pending
       (let* ((tid (plist-get args :tid))
              (ts (my-dida--task-state tid)))
         (when ts
           (my-dida--task-state-set tid (plist-put ts :pending nil)))
         'clear-pending))
      ('remote-gone
       (let ((tid (plist-get args :tid)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--remove-local tid))
         (my-dida--task-state-del tid)
         'remove))
      ('out-of-scope
       (my-dida--exec-out-of-scope args))
      ('forget
       (my-dida--task-state-del (plist-get args :tid))
       'forget)
      ('create-remote
       (my-dida--create-remote (plist-get args :rec))
       'create)
      ('create-local
       (with-current-buffer (my-dida--buffer)
         (my-dida--insert-task (plist-get args :pid) (plist-get args :name)
                               (plist-get args :tid)
                               (plist-get args :remote)
                               (plist-get args :raw-content)))
       (my-dida--task-state-set
        (plist-get args :tid)
        (list :pid (plist-get args :pid) :base (plist-get args :remote)))
       'create)
      ('push-move
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (from (plist-get args :from)))
         (dida-api-move-task tid from pid)
         (my-dida--task-state-set
          tid (list :pid pid
                    :base (ignore-errors
                            (my-dida--api-snapshot (dida-api-get-task pid tid)))))
         'move))
      ('move-local
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (name (plist-get args :name)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--move-local-task tid pid name))
         (let ((ts (my-dida--task-state tid)))
           (my-dida--task-state-set tid (plist-put ts :pid pid)))
         'move))
      ('conflict
       (my-dida--task-state-set
        (plist-get args :tid)
        (list :pid (plist-get args :pid)
              :base (plist-get args :base)
              :conflict (list :reason (plist-get args :reason)
                              :local (plist-get args :local)
                              :remote (plist-get args :remote)
                              :title (plist-get args :title))))
       (my-dida--log "冲突：%s" (or (plist-get args :title) (plist-get args :tid)))
       'conflict)
      ('report-conflict
       (my-dida--log "仍有冲突未解决：%s" (or (plist-get args :title)
                                              (plist-get args :tid)))
       'conflict)
      ('skip
       (my-dida--log "跳过 %s：%s" (or (plist-get args :title) "")
                     (plist-get args :reason))
       'skip)
      ('bulk-freeze
       (my-dida--log "本轮有 %d 个任务从 dida.org 消失，超过阈值；已冻结，请检查"
                     (plist-get args :count))
       'freeze)
      (_ 'skip))))

(defun my-dida--exec-out-of-scope (args)
  "处理从 dida.org 消失的任务。"
  (let ((tid (plist-get args :tid))
        (pid (plist-get args :pid)))
    (pcase my-dida-out-of-scope-policy
      ('delete
       (condition-case err
           (progn
             (dida-api-delete-task pid tid)
             (my-dida--task-state-del tid)
             'delete)
         (error
          (my-dida--log "删除远端任务 %s 失败：%s" tid (error-message-string err))
          (let ((ts (or (my-dida--task-state tid) (list :pid pid))))
            (my-dida--task-state-set tid (plist-put ts :pending-delete t)))
          'delete-pending)))
      ('detach
       (my-dida--task-state-del tid)
       'detach)
      ('review
       (let ((ts (or (my-dida--task-state tid) (list :pid pid))))
         (my-dida--task-state-set
          tid (plist-put ts :conflict
                         (list :reason "out-of-scope"
                               :title (plist-get args :title)))))
       'conflict)
      (_ 'skip))))

(defun my-dida--exec-ops (ops)
  "执行 OPS，返回统计。"
  (let ((counts nil))
    (dolist (op ops)
      (let ((result
             (condition-case err
                 (my-dida--exec op)
               (error
                (my-dida--log "操作 %s 失败：%s" (car op) (error-message-string err))
                'error))))
        (setq counts (plist-put counts result (1+ (or (plist-get counts result) 0))))))
    (with-current-buffer (my-dida--buffer)
      (when (buffer-modified-p) (save-buffer)))
    counts))

;;;; ---------------------------------------------------------------------------
;;;; 8. 命令
;;;; ---------------------------------------------------------------------------

(defun my-dida--log (format-string &rest args)
  (unless my-dida--quiet
    (message "[my-dida] %s" (apply #'format format-string args))))

(defun my-dida--report (counts)
  (let (parts)
    (cl-loop for (key value) on counts by #'cddr
             do (push (format "%s=%s" key value) parts))
    (my-dida--log "同步完成：%s" (if parts (mapconcat #'identity (nreverse parts) " ") "无变化"))))

(defun my-dida--plan-and-run (&optional dry-run)
  (unless (dida-auth--ensure)
    (user-error "[my-dida] 未授权：请先运行 M-x dida-authorize 或配置 token"))
  (if my-dida--busy
      (my-dida--log "上一轮同步还没结束，跳过")
    (setq my-dida--busy t)
    (unwind-protect
        (progn
          (my-dida--state-load)
          (let* ((local (my-dida--collect-local))
                 (remote (my-dida--collect-remote))
                 (ops (my-dida--plan local remote)))
            (if dry-run
                (my-dida--print-preview ops)
              (my-dida--report (my-dida--exec-ops ops))
              (when (plist-get remote :repeats)
                (my-dida--log "跳过 %d 个重复任务（重复任务请在 habit.org 管理）"
                              (length (plist-get remote :repeats))))
              (when (plist-get remote :completed)
                (my-dida--log "忽略 %d 个已完成任务（不会写回 dida.org）"
                              (length (plist-get remote :completed))))
              (setq my-dida--state
                    (plist-put my-dida--state :last-sync (format-time-string "%FT%T%z")))
              (my-dida--state-save))))
      (setq my-dida--busy nil))))

(defun my-dida--print-preview (ops)
  (with-current-buffer (get-buffer-create "*my-dida-preview*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (special-mode)
      (insert "my-dida 同步计划（不写任何东西）\n\n")
      (dolist (op ops)
        (let ((args (cdr op)))
          (insert (format "%-16s %-24s %s\n"
                          (car op)
                          (or (plist-get args :name)
                              (plist-get args :pid)
                              "")
                          (or (plist-get args :title) ""))))))
      (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun my-dida-sync ()
  "完整双向同步。"
  (interactive)
  (setq my-dida--quiet nil)
  (my-dida--plan-and-run))

;;;###autoload
(defun my-dida-preview ()
  "只显示同步计划，不写任何东西。"
  (interactive)
  (my-dida--plan-and-run t))

(defun my-dida--quiet-sync ()
  (let ((my-dida--quiet t))
    (condition-case err
        (my-dida--plan-and-run)
      (error
       (message "[my-dida] 后台同步失败：%s" (error-message-string err))))))

(defun my-dida--schedule-sync (&optional seconds)
  (when my-dida--debounce-timer
    (cancel-timer my-dida--debounce-timer))
  (setq my-dida--debounce-timer
        (run-at-time (or seconds my-dida-debounce) nil #'my-dida--quiet-sync)))

(defun my-dida--in-scope-buffer-p ()
  (and (buffer-file-name)
       (string= (file-truename (buffer-file-name))
                (file-truename (my-dida--file)))))

(defun my-dida--after-save ()
  (when (my-dida--in-scope-buffer-p)
    (my-dida--schedule-sync)))

(defun my-dida--after-todo-change ()
  (when (my-dida--in-scope-buffer-p)
    (my-dida--schedule-sync)))

(defun my-dida--idle-sync ()
  (when (and (not my-dida--busy)
             (if (functionp my-dida-primary-device-p)
                 (funcall my-dida-primary-device-p)
               my-dida-primary-device-p))
    (my-dida--quiet-sync)))

;;;###autoload
(define-minor-mode my-dida-auto-sync-mode
  "全局自动同步：启动/空闲/保存/状态变化时同步 dida.org。"
  :global t
  :lighter " my-dida"
  (if my-dida-auto-sync-mode
      (progn
        (add-hook 'after-save-hook #'my-dida--after-save)
        (add-hook 'org-after-todo-state-change-hook #'my-dida--after-todo-change)
        (setq my-dida--idle-timer
              (run-with-idle-timer my-dida-idle-interval t #'my-dida--idle-sync))
        (run-at-time my-dida-auto-sync-startup-delay nil #'my-dida--quiet-sync))
    (remove-hook 'after-save-hook #'my-dida--after-save)
    (remove-hook 'org-after-todo-state-change-hook #'my-dida--after-todo-change)
    (when my-dida--idle-timer
      (cancel-timer my-dida--idle-timer)
      (setq my-dida--idle-timer nil))
    (when my-dida--debounce-timer
      (cancel-timer my-dida--debounce-timer)
      (setq my-dida--debounce-timer nil))))

;;;###autoload
(defun my-dida-push-heading ()
  "立即把当前 heading 推到滴答（新建或更新）。"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((tid (org-entry-get nil "DIDA_TID"))
           (pid (org-entry-get nil "DIDA_PID" t))
           (snapshot (my-dida--org-snapshot)))
      (unless pid (user-error "当前任务不在任何清单 heading 下"))
      (if tid
          (progn
            (my-dida--push-snapshot tid pid snapshot)
            (my-dida--set-state tid pid snapshot)
            (message "已推送 %s" tid))
        (let* ((payload (my-dida--create-payload pid snapshot
                                                 (my-dida--direct-body)))
               (task (dida-api-create-task payload))
               (new-tid (and task (alist-get 'id task))))
          (unless new-tid (user-error "创建结果未知"))
          (org-set-property "DIDA_TID" new-tid)
          (org-set-property "DIDA_BASE"
                            (my-dida--snapshot-hash (my-dida--api-snapshot task)))
          (my-dida--state-load)
          (my-dida--task-state-set new-tid
                                   (list :pid (or (alist-get 'projectId task) pid)
                                         :base (my-dida--api-snapshot task)))
          (my-dida--state-save)
          (save-buffer)
          (message "已创建 %s" new-tid))))))

;;;###autoload
(defun my-dida-detach ()
  "去掉当前 heading 的 DIDA_* 属性与本地同步状态，远端不动。"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((tid (org-entry-get nil "DIDA_TID")))
      (when tid
        (my-dida--state-load)
        (my-dida--task-state-del tid)
        (my-dida--state-save))
      (dolist (prop '("DIDA_TID" "DIDA_BASE" "DIDA_LIST" "DIDA_OFF"))
        (org-delete-property prop))
      (message "已脱离同步%s" (if tid (format "（%s）" tid) "")))))

;;;###autoload
(defun my-dida-set-list ()
  "把当前任务换到另一个滴答清单（下一次同步推 move）。"
  (interactive)
  (let* ((projects (dida-api-list-projects))
         (choices (mapcar (lambda (p) (cons (alist-get 'name p) (alist-get 'id p)))
                          projects))
         (name (completing-read "清单：" (mapcar #'car choices) nil t))
         (pid (cdr (assoc name choices))))
    (org-entry-put nil "DIDA_PID" pid)
    (org-entry-put nil "DIDA_LIST" name)
    (message "已改为清单 %s，下次同步会推送移动" name)))

;;;; ---------------------------------------------------------------------------
;;;; 9. 冲突审查
;;;; ---------------------------------------------------------------------------

;;;###autoload
(defun my-dida-review ()
  "查看冲突/待确认删除。"
  (interactive)
  (my-dida--state-load)
  (let ((items (cl-remove-if-not (lambda (pair) (plist-get (cdr pair) :conflict))
                                 (plist-get my-dida--state :tasks))))
    (with-current-buffer (get-buffer-create "*my-dida-review*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert "my-dida 待处理（按 q 退出，处理完可再跑 my-dida-sync）\n\n")
        (if (null items)
            (insert "没有待处理项。\n")
          (dolist (pair items)
            (let* ((tid (car pair))
                   (conflict (plist-get (cdr pair) :conflict))
                   (reason (plist-get conflict :reason))
                   (title (or (plist-get conflict :title) tid)))
              (insert (format "任务：%s [%s]\n原因：%s\n" title tid reason))
              (if (eq reason 'out-of-scope)
                  (progn
                    (insert-text-button "删除滴答任务"
                                        'action (lambda (_) (my-dida--resolve tid 'delete))
                                        'follow-link t)
                    (insert "  ")
                    (insert-text-button "保留滴答任务（本地脱钩）"
                                        'action (lambda (_) (my-dida--resolve tid 'detach))
                                        'follow-link t))
                (insert-text-button "以本地为准"
                                    'action (lambda (_) (my-dida--resolve tid 'local))
                                    'follow-link t)
                (insert "  ")
                (insert-text-button "以滴答为准"
                                    'action (lambda (_) (my-dida--resolve tid 'remote))
                                    'follow-link t))
              (insert "\n\n")))))
      (pop-to-buffer (current-buffer)))))

(defun my-dida--local-snapshot-if-any (tid)
  "返回 (POS . SNAPSHOT)，本地没有 TID 时返回 nil。"
  (with-current-buffer (my-dida--buffer)
    (when-let ((pos (my-dida--find-tid tid)))
      (goto-char pos)
      (cons pos (my-dida--org-snapshot)))))

(defun my-dida--resolve (tid choice)
  "解决 TID 的冲突。CHOICE 为 local / remote / delete / detach。"
  (my-dida--state-load)
  (let* ((ts (my-dida--task-state tid))
         (conflict (plist-get ts :conflict))
         (pid (plist-get ts :pid))
         (reason (plist-get conflict :reason)))
    (unless ts (user-error "没有这个任务的同步状态：%s" tid))
    (pcase choice
      ('detach
       (my-dida--task-state-del tid))
      ('delete
       (when pid (ignore-errors (dida-api-delete-task pid tid)))
       (my-dida--task-state-del tid))
      ('local
       (if (eq reason 'out-of-scope)
           (progn
             (when pid (ignore-errors (dida-api-delete-task pid tid)))
             (my-dida--task-state-del tid))
         (let* ((local (my-dida--local-snapshot-if-any tid))
                (pos (car-safe local))
                (snapshot (cdr-safe local)))
           (cond
            ((null local)
             (when pid (ignore-errors (dida-api-delete-task pid tid)))
             (my-dida--task-state-del tid))
            ((and pid (dida-api-get-task pid tid))
             (my-dida--push-snapshot tid pid snapshot)
             (my-dida--set-state tid pid snapshot))
            (t
             ;; 远端已不存在：用本地重建一条
             (let* ((body (with-current-buffer (my-dida--buffer)
                            (goto-char pos)
                            (my-dida--direct-body)))
                    (task (dida-api-create-task
                           (my-dida--create-payload pid snapshot body)))
                    (new-tid (and task (alist-get 'id task))))
               (unless new-tid (error "重建远端任务失败"))
               (with-current-buffer (my-dida--buffer)
                 (goto-char pos)
                 (org-set-property "DIDA_TID" new-tid)
                 (org-set-property "DIDA_BASE"
                                   (my-dida--snapshot-hash (my-dida--api-snapshot task))))
               (my-dida--task-state-del tid)
               (my-dida--task-state-set
                new-tid (list :pid (or (alist-get 'projectId task) pid)
                              :base (my-dida--api-snapshot task)))))))))
      ('remote
       (if (eq reason 'out-of-scope)
           (my-dida--task-state-del tid)
         (let ((remote (and pid (dida-api-get-task pid tid))))
           (if remote
               (let ((snapshot (my-dida--api-snapshot remote)))
                 (with-current-buffer (my-dida--buffer)
                   (my-dida--apply-snapshot tid snapshot))
                 (my-dida--set-state tid pid snapshot))
             (with-current-buffer (my-dida--buffer)
               (my-dida--remove-local tid))
             (my-dida--task-state-del tid)))))
      (_ (user-error "未知选择：%s" choice)))
    (my-dida--state-save)
    (with-current-buffer (my-dida--buffer)
      (when (buffer-modified-p) (save-buffer)))
    (message "已处理 %s" tid)
    (my-dida-review)))

(provide 'dida-workflow)
;;; dida-workflow.el ends here
