;;; dida-workflow.el --- 在 dida.el API 之上实现「日常清单」双向同步 -*- lexical-binding: t; -*-

;; Author: 由 dida.el 用户维护的自定义工作流层
;; Version: 0.2
;; Keywords: org, convenience
;; Package-Requires: ((emacs "27.1") (dida "2.4"))
;;
;; 本文件随 dida 包一起分发（同一仓库、TomoELPA 的 dida 包会把它装到包目录），
;; 只使用 dida.el 的 API 层（2.4 起为全异步，网络等待不阻塞 Emacs）：
;;   认证：dida-auth--ensure-async / dida-token
;;   请求：dida-api--request-async（内部由 dida-api-*-async 封装）
;;   资源：dida-api-list-projects-async / dida-api-get-project-data-async /
;;         dida-api-get-task-async / dida-api-create-task-async /
;;         dida-api-update-task-async / dida-api-complete-task-async /
;;         dida-api-delete-task-async / dida-api-move-task-async /
;;         dida-api-task-payload / dida-api-payload-from-remote
;;
;; 工作流模型（详见 WORKFLOW-RESET.org）：
;;   - dida.org：一级清单 heading + 二级任务（任务为叶子），与滴答双向；
;;   - 状态只有 待办 / 完成 / 放弃；完成与放弃在滴答侧都算完成；
;;   - 任务标题 = heading 去掉 TODO 关键字与优先级后的原文，:tag: 标签、
;;     [/] 统计 cookie 等按文本原样双向同步；
;;   - 滴答只接收标题、正文和 SCHEDULED 时间：优先级、DEADLINE 等 Org
;;     特有属性不映射（本地保留，推拉都不动）；
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
  '(:title :content :status :due :all-day))

(defun my-dida--content-p ()
  "正文是否参与双向比较。"
  (eq my-dida-content-policy 'two-way))

(defun my-dida--content-push-p ()
  "推远端时是否覆盖正文。"
  (eq my-dida-content-policy 'two-way))

(defun my-dida--norm-text (s)
  "把 S 归一成用于同步比较的标题文本。
去掉首尾空白并剥掉 Emacs 文本属性：Org 解析出的标题常带
`org-category'/`org-stats' 等属性，而远端标题是纯文本；属性会进入
`my-dida--snapshot-hash'（基于 `prin1-to-string'），导致两边指纹永远
不同，同步在 push/pull 之间来回抖动。"
  (substring-no-properties (string-trim (or s ""))))

(defun my-dida--norm-content (s)
  (let* ((s (or s ""))
         (s (replace-regexp-in-string "\r\n?" "\n" s)))
    (string-trim s)))

(defun my-dida--heading-title ()
  "当前 heading 的标题原文。

去掉 TODO 关键字和优先级 cookie，其余文本（含 :tag: 标签、[/] 统计
cookie、链接、强调标记等）原样返回，作为同步到滴答的任务标题。"
  (let* ((element (org-element-at-point))
         (title (my-dida--norm-text (org-element-property :title element)))
         (tags (org-element-property :tags element)))
    (if tags
        (my-dida--norm-text (concat title " " (org-make-tag-string tags)))
      title)))

(defun my-dida--split-title-tags (title)
  "把标题原文 TITLE 拆成 (BASE . TAGS)。

TITLE 末尾符合 Org 标签语法的 :tag1:tag2: 视为标签；BASE 是去掉这段
标签的标题文本。没有标签时 TAGS 为 nil。"
  (let* ((title (my-dida--norm-text title))
         ;; 补一个空格，让 TITLE 本身以标签开头时也能匹配 org-tag-group-re。
         (padded (concat " " title)))
    (if (string-match (concat org-tag-group-re "\\'") padded)
        (cons (my-dida--norm-text (substring padded 0 (match-beginning 0)))
              (split-string (match-string 2 padded) ":" t))
      (cons title nil))))

(defun my-dida--set-heading-title (title)
  "把当前 heading 的标题设为原文 TITLE。

TITLE 末尾的 :tag: 会写成真正的 Org 标签；其余文本（含 [/]、链接等）
原样写入标题。"
  (let* ((split (my-dida--split-title-tags title))
         (base (car split))
         (tags (cdr split)))
    (org-edit-headline base)
    (org-set-tags tags)))

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
  "当前 heading 的快照。

只取标题原文、正文、状态和 SCHEDULED 时间：优先级、DEADLINE、标签到
滴答标签的映射等 Org 特有属性都不参与同步。"
  (let* ((done (org-entry-is-done-p))
         (status (if done 2 0))
         (cancelled (and (= status 2)
                         (equal (org-get-todo-state) my-dida-cancelled-keyword))))
    (list :title (my-dida--heading-title)
          :content (if (my-dida--content-p)
                       (my-dida--norm-content (my-dida--direct-body))
                     "")
          :status status
          :due (my-dida--org-due)
          :all-day (my-dida--org-all-day-p)
          :cancelled cancelled)))

(defun my-dida--org-due ()
  "当前 heading 的 SCHEDULED 时间，返回 YYYY-MM-DD[THH:MM]。
DEADLINE 不参与同步。"
  (let ((ts (org-element-property :scheduled (org-element-at-point))))
    (when ts
      (let ((time (org-timestamp-to-time ts)))
        (if (org-element-property :hour-start ts)
            (format-time-string "%Y-%m-%dT%H:%M" time)
          (format-time-string "%Y-%m-%d" time))))))

(defun my-dida--org-all-day-p ()
  "当前 heading 的 SCHEDULED 是否为全天；DEADLINE 不参与同步。"
  (let ((ts (org-element-property :scheduled (org-element-at-point))))
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
       (unless (equal title (my-dida--heading-title))
         (my-dida--set-heading-title title)))
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
     ;; 日期：只写 SCHEDULED，不动本地 DEADLINE 与优先级
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
                 (title (my-dida--heading-title)))
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

(defun my-dida--collect-remote-async (callback)
  "异步拉取远端清单与未完成任务。

全部收齐后以 (:names :by-tid :repeats :completed) 调用 CALLBACK；
失败时以 nil 加错误数据调用。
`project/{pid}/data' 正常只返回未完成任务；这里仍显式忽略 status 2/-1，
避免服务端行为变化时把已完成任务当成「远端新任务」重新插回 dida.org。"
  (dida-api-list-projects-async
   (lambda (projects err)
     (cond
      (err (funcall callback nil err))
      ((null projects)
       (funcall callback nil
                (dida-api--error-data
                 'client :message "远端没有返回任何清单，已中止")))
      (t
       (let ((by-tid nil)
             (names nil)
             (repeats nil)
             (completed nil)
             (seen nil)
             (remaining nil)
             (done nil))
         (dolist (project projects)
           (when (my-dida--project-selected-p (alist-get 'id project)
                                              (alist-get 'name project))
             (push (cons (alist-get 'id project) (alist-get 'name project)) names)
             (push project remaining)))
         (setq remaining (nreverse remaining))
         (cl-labels
             ((finish ()
                (unless done
                  (setq done t)
                  (setq my-dida--remote-names names)
                  (funcall callback
                           (list :names names
                                 :by-tid by-tid
                                 :repeats (nreverse repeats)
                                 :completed (nreverse completed))
                           nil)))
              (fail (err)
                (unless done
                  (setq done t)
                  (funcall callback nil err)))
              (collect-task (task pid name)
                "记录一个远端任务；返回重复的 TID 或 nil。"
                (let ((tid (alist-get 'id task)))
                  (cond
                   ((member tid seen) tid)
                   (t
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
                        (push (cons tid
                                    (list :pid pid
                                          :name name
                                          :task task
                                          :repeat nil
                                          :raw-content (or (alist-get 'content task) "")
                                          :snapshot (my-dida--api-snapshot task)))
                              by-tid)))
                      nil)))))
              (collect-project (data pid name)
                "记录项目 DATA 的全部任务；返回重复的 TID 或 nil。"
                (catch 'dup
                  (dolist (task (append (alist-get 'tasks data) nil))
                    (let ((dup (collect-task task pid name)))
                      (when dup (throw 'dup dup))))
                  nil))
              (step ()
                (if (null remaining)
                    (finish)
                  (let* ((project (car remaining))
                         (pid (alist-get 'id project))
                         (name (alist-get 'name project)))
                    (setq remaining (cdr remaining))
                    (dida-api-get-project-data-async
                     pid
                     (lambda (data derr)
                       (if derr
                           (fail derr)
                         (let ((dup (collect-project data pid name)))
                           (if dup
                               (fail (dida-api--error-data
                                      'client
                                      :message (format "远端任务 id 重复：%s" dup)))
                             (step))))))))))
           (step))))))))


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
    ;; LOCAL-PROJECTS 的元素是 (:pid PID :name NAME) plist，不是 (PID . NAME)，
    ;; 不能直接用 `assoc' 比较 pid，否则每轮都会生成 ensure-project 空操作。
    (dolist (pair (plist-get remote :names))
      (unless (cl-find-if (lambda (project)
                            (equal (car pair) (plist-get project :pid)))
                          local-projects)
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

(defun my-dida--complete-remote (tid pid callback)
  "可靠完成任务，完成后以 t/nil 调用 CALLBACK。"
  (let ((failed (lambda (msg)
                  (my-dida--log "完成任务 %s 失败：%s" tid (or msg "未知错误"))
                  (funcall callback nil))))
    (dida-api-complete-task-async
     pid tid
     (lambda (_result err)
       (if err
           (funcall failed (plist-get err :message))
         (dida-api-get-task-async
          pid tid
          (lambda (fresh gerr)
            (cond
             (gerr (funcall failed (plist-get gerr :message)))
             ((null fresh) (funcall callback t))
             ((= (or (alist-get 'status fresh) 0) 2) (funcall callback t))
             (t
              (let ((payload (dida-api-payload-from-remote fresh)))
                (setq payload (plist-put payload :status 2))
                (dida-api-update-task-async
                 tid payload
                 (lambda (_data uerr)
                   (if uerr
                       (funcall failed (plist-get uerr :message))
                     (dida-api-get-task-async
                      pid tid
                      (lambda (again aerr)
                        (if aerr
                            (funcall failed (plist-get aerr :message))
                          (funcall callback
                                   (or (null again)
                                       (= (or (alist-get 'status again) 0) 2)))))))))))))))))))

(defun my-dida--push-snapshot (tid pid snapshot callback)
  "把 SNAPSHOT 推到远端 TID，完成后以 t/nil 调用 CALLBACK。"
  (dida-api-get-task-async
   pid tid
   (lambda (remote err)
     (cond
      (err
       (my-dida--log "读取远端任务 %s 失败：%s" tid
                     (or (plist-get err :message) "未知错误"))
       (funcall callback nil))
      ((null remote)
       (my-dida--log "远端任务 %s 不存在" tid)
       (funcall callback nil))
      (t
       (let ((payload (dida-api-payload-from-remote remote)))
         (setq payload (plist-put payload :title (or (plist-get snapshot :title) "")))
         (when (my-dida--content-push-p)
           (setq payload (plist-put payload :content
                                    (or (plist-get snapshot :content) ""))))
         (setq payload (plist-put payload :status 0))
         (if (plist-get snapshot :due)
             (progn
               (setq payload (plist-put payload :dueDate (my-dida--due->api snapshot)))
               (setq payload (plist-put payload :isAllDay
                                        (if (plist-get snapshot :all-day)
                                            t :json-false))))
           (cl-remf payload :dueDate)
           (cl-remf payload :isAllDay))
         (dida-api-update-task-async
          tid payload
          (lambda (_data uerr)
            (if uerr
                (progn
                  (my-dida--log "推送任务 %s 失败：%s" tid
                                (or (plist-get uerr :message) "未知错误"))
                  (funcall callback nil))
              (funcall callback t))))))))))


(defun my-dida--create-payload (pid snapshot content)
  "构造创建任务的 payload：只发标题、正文和时间，不发优先级等 Org 特有属性。"
  (let ((due (my-dida--due->api snapshot)))
    (dida-api-task-payload
     :title (or (plist-get snapshot :title) "")
     :project-id pid
     :content (if (eq my-dida-content-policy 'off) "" content)
     :due-date due
     :is-all-day (and due (if (plist-get snapshot :all-day) t :json-false)))))

(defun my-dida--create-remote (rec callback)
  "把本地新任务创建到滴答，完成后以新 TID 或 nil 调用 CALLBACK。"
  (let* ((pid (plist-get rec :pid))
         (snapshot (plist-get rec :snapshot))
         (payload (my-dida--create-payload pid snapshot
                                           (plist-get rec :raw-content))))
    (dida-api-create-task-async
     payload
     (lambda (task err)
       (cond
        (err
         (my-dida--log "创建任务失败：%s" (or (plist-get err :message) "未知错误"))
         (funcall callback nil))
        ((not (and task (alist-get 'id task)))
         (my-dida--log "创建任务结果未知")
         (funcall callback nil))
        (t
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
           (funcall callback tid))))))))


(defun my-dida--set-state (tid pid snapshot)
  (my-dida--task-state-set tid (list :pid pid :base snapshot))
  (with-current-buffer (my-dida--buffer)
    (my-dida--set-base-property tid (my-dida--snapshot-hash snapshot))))

(defun my-dida--exec (op callback)
  "执行一个操作，完成后以结果符号调用 CALLBACK。

网络类 op（push / merge / local-done / pending-complete / create-remote /
push-move / out-of-scope 的 delete）走异步，纯本地 op 同步执行后立即回调。"
  (let ((action (car op))
        (args (cdr op)))
    (pcase action
      ('ensure-project
       (with-current-buffer (my-dida--buffer)
         (my-dida--ensure-project (plist-get args :pid) (plist-get args :name)))
       (funcall callback 'project))
      ('agree
       (my-dida--set-state (plist-get args :tid) (plist-get args :pid)
                           (plist-get args :remote))
       (funcall callback 'agree))
      ('pull
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (remote (plist-get args :remote)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--apply-snapshot tid remote))
         (my-dida--set-state tid pid remote)
         (funcall callback 'pull)))
      ('push
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (local (plist-get args :local)))
         (my-dida--push-snapshot
          tid pid local
          (lambda (ok)
            (when ok
              (my-dida--set-state tid pid local))
            (funcall callback (if ok 'push 'error))))))
      ('merge
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (target (plist-get args :target)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--apply-snapshot tid target))
         (my-dida--push-snapshot
          tid pid target
          (lambda (ok)
            (when ok
              (my-dida--set-state tid pid target))
            (funcall callback (if ok 'merge 'error))))))
      ('local-done
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid)))
         (my-dida--complete-remote
          tid pid
          (lambda (ok)
            (if ok
                (progn
                  (with-current-buffer (my-dida--buffer)
                    (my-dida--remove-local tid))
                  (my-dida--task-state-del tid)
                  (funcall callback 'complete))
              (my-dida--task-state-set
               tid (plist-put (my-dida--task-state tid) :pending t))
              (funcall callback 'complete-pending))))))
      ('pending-complete
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid)))
         (my-dida--complete-remote
          tid pid
          (lambda (ok)
            (if ok
                (progn
                  (with-current-buffer (my-dida--buffer)
                    (my-dida--remove-local tid))
                  (my-dida--task-state-del tid)
                  (funcall callback 'complete))
              (funcall callback 'complete-pending))))))
      ('clear-pending
       (let* ((tid (plist-get args :tid))
              (ts (my-dida--task-state tid)))
         (when ts
           (my-dida--task-state-set tid (plist-put ts :pending nil)))
         (funcall callback 'clear-pending)))
      ('remote-gone
       (let ((tid (plist-get args :tid)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--remove-local tid))
         (my-dida--task-state-del tid)
         (funcall callback 'remove)))
      ('out-of-scope
       (my-dida--exec-out-of-scope args callback))
      ('forget
       (my-dida--task-state-del (plist-get args :tid))
       (funcall callback 'forget))
      ('create-remote
       (my-dida--create-remote
        (plist-get args :rec)
        (lambda (new-tid)
          (funcall callback (if new-tid 'create 'error)))))
      ('create-local
       (with-current-buffer (my-dida--buffer)
         (my-dida--insert-task (plist-get args :pid) (plist-get args :name)
                               (plist-get args :tid)
                               (plist-get args :remote)
                               (plist-get args :raw-content)))
       (my-dida--task-state-set
        (plist-get args :tid)
        (list :pid (plist-get args :pid) :base (plist-get args :remote)))
       (funcall callback 'create))
      ('push-move
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (from (plist-get args :from)))
         (dida-api-move-task-async
          tid from pid
          (lambda (_data merr)
            (if merr
                (progn
                  (my-dida--log "移动任务 %s 失败：%s" tid
                                (or (plist-get merr :message) "未知错误"))
                  (funcall callback 'error))
              (dida-api-get-task-async
               pid tid
               (lambda (task _gerr)
                 (my-dida--task-state-set
                  tid (list :pid pid
                            :base (ignore-errors
                                    (my-dida--api-snapshot task))))
                 (funcall callback 'move))))))))
      ('move-local
       (let ((tid (plist-get args :tid))
             (pid (plist-get args :pid))
             (name (plist-get args :name)))
         (with-current-buffer (my-dida--buffer)
           (my-dida--move-local-task tid pid name))
         (let ((ts (my-dida--task-state tid)))
           (my-dida--task-state-set tid (plist-put ts :pid pid)))
         (funcall callback 'move)))
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
       (funcall callback 'conflict))
      ('report-conflict
       (my-dida--log "仍有冲突未解决：%s" (or (plist-get args :title)
                                              (plist-get args :tid)))
       (funcall callback 'conflict))
      ('skip
       (my-dida--log "跳过 %s：%s" (or (plist-get args :title) "")
                     (plist-get args :reason))
       (funcall callback 'skip))
      ('bulk-freeze
       (my-dida--log "本轮有 %d 个任务从 dida.org 消失，超过阈值；已冻结，请检查"
                     (plist-get args :count))
       (funcall callback 'freeze))
      (_ (funcall callback 'skip)))))

(defun my-dida--exec-out-of-scope (args callback)
  "处理从 dida.org 消失的任务，完成后以结果符号调用 CALLBACK。"
  (let ((tid (plist-get args :tid))
        (pid (plist-get args :pid)))
    (pcase my-dida-out-of-scope-policy
      ('delete
       (dida-api-delete-task-async
        pid tid
        (lambda (_data err)
          (if err
              (progn
                (my-dida--log "删除远端任务 %s 失败：%s" tid
                              (or (plist-get err :message) "未知错误"))
                (let ((ts (or (my-dida--task-state tid) (list :pid pid))))
                  (my-dida--task-state-set tid (plist-put ts :pending-delete t)))
                (funcall callback 'delete-pending))
            (my-dida--task-state-del tid)
            (funcall callback 'delete)))))
      ('detach
       (my-dida--task-state-del tid)
       (funcall callback 'detach))
      ('review
       (let ((ts (or (my-dida--task-state tid) (list :pid pid))))
         (my-dida--task-state-set
          tid (plist-put ts :conflict
                         (list :reason "out-of-scope"
                               :title (plist-get args :title)))))
       (funcall callback 'conflict))
      (_ (funcall callback 'skip)))))

(defun my-dida--exec-ops (ops callback)
  "异步串行执行 OPS，全部结束后以统计 plist 调用 CALLBACK。

单个 op 失败时保持现有语义：记录 error、继续下一个；全部结束后保存 dida.org。"
  (let ((counts nil)
        (remaining ops))
    (cl-labels
        ((finish ()
           (with-current-buffer (my-dida--buffer)
             (when (buffer-modified-p) (save-buffer)))
           (funcall callback counts))
         (count-result (result)
           (setq counts (plist-put counts result
                                   (1+ (or (plist-get counts result) 0)))))
         (step ()
           (if (null remaining)
               (finish)
             (let ((op (car remaining)))
               (setq remaining (cdr remaining))
               (condition-case err
                   (my-dida--exec
                    op
                    (lambda (result)
                      (condition-case cb-err
                          (progn
                            (count-result result)
                            (step))
                        (error
                         (my-dida--log "同步执行出错：%s"
                                       (error-message-string cb-err))
                         (count-result 'error)
                         (finish)))))
                 (error
                  (my-dida--log "操作 %s 失败：%s" (car op)
                                (error-message-string err))
                  (count-result 'error)
                  (step)))))))
      (step))))

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

(defun my-dida--error-text (err)
  "把回调错误数据或 Lisp 错误对象 ERR 转成可读文本。"
  (cond
   ((null err) nil)
   ((stringp err) err)
   ((and (consp err) (keywordp (car err)))
    (or (plist-get err :message) (format "%s" err)))
   ((consp err) (error-message-string err))
   (t (format "%s" err))))

(defun my-dida--plan-and-run (&optional dry-run)
  "启动一轮同步（网络全程异步，等待期间不阻塞 Emacs）。

DRY-RUN 非 nil 时只显示计划、不写任何东西。
`my-dida--busy' 在所有成功/失败/异常路径上都会被清理。"
  (if my-dida--busy
      (my-dida--log "上一轮同步还没结束，跳过")
    (setq my-dida--busy t)
    (let ((finished nil))
      (cl-labels
          ((finish (&optional err)
             (unless finished
               (setq finished t)
               (when err
                 (let ((msg (my-dida--error-text err)))
                   (if my-dida--quiet
                       (message "[my-dida] 后台同步失败：%s" msg)
                     (my-dida--log "%s" msg))))
               (setq my-dida--busy nil)))
           (run-exec (local remote)
             (condition-case err
                 (let ((ops (my-dida--plan local remote)))
                   (if dry-run
                       (progn
                         (my-dida--print-preview ops)
                         (finish))
                     (my-dida--exec-ops
                      ops
                      (lambda (counts)
                        (condition-case cb-err
                            (progn
                              (my-dida--report counts)
                              (when (plist-get remote :repeats)
                                (my-dida--log
                                 "跳过 %d 个重复任务（重复任务请在 habit.org 管理）"
                                 (length (plist-get remote :repeats))))
                              (when (plist-get remote :completed)
                                (my-dida--log
                                 "忽略 %d 个已完成任务（不会写回 dida.org）"
                                 (length (plist-get remote :completed))))
                              (setq my-dida--state
                                    (plist-put my-dida--state
                                               :last-sync
                                               (format-time-string "%FT%T%z")))
                              (my-dida--state-save)
                              (finish))
                          (error (finish cb-err)))))))
               (error (finish err))))
           (run ()
             (condition-case err
                 (dida-auth--ensure-async
                  (lambda (ready)
                    (if (not ready)
                        (finish (dida-api--error-data
                                 'auth
                                 :message "未授权：请先运行 M-x dida-authorize 或配置 token"))
                      (condition-case err
                          (progn
                            (my-dida--state-load)
                            (let ((local (my-dida--collect-local)))
                              (my-dida--collect-remote-async
                               (lambda (remote rerr)
                                 (cond
                                  (rerr (finish rerr))
                                  ;; 执行阶段前再检查一次：等待网络期间用户可能
                                  ;; 改过 dida.org，不能用过期快照覆盖编辑。
                                  ((with-current-buffer (my-dida--buffer)
                                     (buffer-modified-p))
                                   (finish
                                    (dida-api--error-data
                                     'client
                                     :message "dida.org 有未保存修改，已取消本轮")))
                                  (t (run-exec local remote)))))))
                        (error (finish err))))))
               (error (finish err)))))
        (run)))))

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
           (snapshot (my-dida--org-snapshot))
           (buffer (current-buffer))
           (pos (point)))
      (unless pid (user-error "当前任务不在任何清单 heading 下"))
      (if tid
          (my-dida--push-snapshot
           tid pid snapshot
           (lambda (ok)
             (if ok
                 (progn
                   (my-dida--set-state tid pid snapshot)
                   (message "已推送 %s" tid))
               (message "推送 %s 失败" tid))))
        (let ((payload (my-dida--create-payload pid snapshot
                                                (my-dida--direct-body))))
          (dida-api-create-task-async
           payload
           (lambda (task err)
             (cond
              ((or err (null task) (null (alist-get 'id task)))
               (message "创建结果未知%s"
                        (if err
                            (format "：%s" (or (plist-get err :message) "未知错误"))
                          "")))
              (t
               (let* ((new-tid (alist-get 'id task))
                      (remote-snapshot (my-dida--api-snapshot task)))
                 (with-current-buffer buffer
                   (save-excursion
                     (goto-char pos)
                     (org-back-to-heading t)
                     (org-set-property "DIDA_TID" new-tid)
                     (org-set-property "DIDA_BASE"
                                       (my-dida--snapshot-hash remote-snapshot))))
                 (my-dida--state-load)
                 (my-dida--task-state-set
                  new-tid
                  (list :pid (or (alist-get 'projectId task) pid)
                        :base remote-snapshot))
                 (my-dida--state-save)
                 (with-current-buffer buffer
                   (when (buffer-modified-p) (save-buffer)))
                 (message "已创建 %s" new-tid)))))))))))


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
  (let ((buffer (current-buffer))
        (pos (point)))
    (dida-api-list-projects-async
     (lambda (projects err)
       (if err
           (message "[my-dida] 获取清单失败：%s"
                    (or (plist-get err :message) "未知错误"))
         (let* ((choices (mapcar (lambda (project)
                                   (cons (alist-get 'name project)
                                         (alist-get 'id project)))
                                 projects))
                (name (completing-read "清单：" (mapcar #'car choices) nil t))
                (pid (cdr (assoc name choices))))
           (with-current-buffer buffer
             (save-excursion
               (goto-char pos)
               (org-entry-put nil "DIDA_PID" pid)
               (org-entry-put nil "DIDA_LIST" name)))
           (message "已改为清单 %s，下次同步会推送移动" name)))))))


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
    (cl-labels
        ((finish ()
           (my-dida--state-save)
           (with-current-buffer (my-dida--buffer)
             (when (buffer-modified-p) (save-buffer)))
           (message "已处理 %s" tid)
           (my-dida-review))
         (fail (err)
           (message "[my-dida] 处理 %s 失败：%s" tid
                    (or (and (consp err) (plist-get err :message))
                        "未知错误")))
         (delete-remote (then)
           (if pid
               (dida-api-delete-task-async
                pid tid
                ;; 与同步版一致：删除失败也继续清理本地状态（ignore-errors）
                (lambda (_data _err) (funcall then)))
             (funcall then)))
         (recreate (pos snapshot)
           (let* ((body (with-current-buffer (my-dida--buffer)
                          (save-excursion
                            (goto-char pos)
                            (my-dida--direct-body))))
                  (payload (my-dida--create-payload pid snapshot body)))
             (dida-api-create-task-async
              payload
              (lambda (task err)
                (if (or err (null task) (null (alist-get 'id task)))
                    (fail err)
                  (let ((new-tid (alist-get 'id task))
                        (remote-snapshot (my-dida--api-snapshot task)))
                    (with-current-buffer (my-dida--buffer)
                      (save-excursion
                        (goto-char pos)
                        (org-set-property "DIDA_TID" new-tid)
                        (org-set-property "DIDA_BASE"
                                          (my-dida--snapshot-hash remote-snapshot))))
                    (my-dida--task-state-del tid)
                    (my-dida--task-state-set
                     new-tid
                     (list :pid (or (alist-get 'projectId task) pid)
                           :base remote-snapshot))
                    (finish))))))))
      (pcase choice
        ('detach
         (my-dida--task-state-del tid)
         (finish))
        ('delete
         (delete-remote (lambda ()
                          (my-dida--task-state-del tid)
                          (finish))))
        ('local
         (if (eq reason 'out-of-scope)
             (delete-remote (lambda ()
                              (my-dida--task-state-del tid)
                              (finish)))
           (let* ((local (my-dida--local-snapshot-if-any tid))
                  (pos (car-safe local))
                  (snapshot (cdr-safe local)))
             (cond
              ((null local)
               (delete-remote (lambda ()
                                (my-dida--task-state-del tid)
                                (finish))))
              ((null pid)
               (recreate pos snapshot))
              (t
               (dida-api-get-task-async
                pid tid
                (lambda (remote gerr)
                  (cond
                   (gerr (fail gerr))
                   (remote
                    (my-dida--push-snapshot
                     tid pid snapshot
                     (lambda (ok)
                       (if ok
                           (progn
                             (my-dida--set-state tid pid snapshot)
                             (finish))
                         (fail nil)))))
                   (t (recreate pos snapshot))))))))))
        ('remote
         (if (eq reason 'out-of-scope)
             (progn
               (my-dida--task-state-del tid)
               (finish))
           (if (null pid)
               (progn
                 (with-current-buffer (my-dida--buffer)
                   (my-dida--remove-local tid))
                 (my-dida--task-state-del tid)
                 (finish))
             (dida-api-get-task-async
              pid tid
              (lambda (remote gerr)
                (cond
                 (gerr (fail gerr))
                 (remote
                  (let ((snapshot (my-dida--api-snapshot remote)))
                    (with-current-buffer (my-dida--buffer)
                      (my-dida--apply-snapshot tid snapshot))
                    (my-dida--set-state tid pid snapshot)
                    (finish)))
                 (t
                  (with-current-buffer (my-dida--buffer)
                    (my-dida--remove-local tid))
                  (my-dida--task-state-del tid)
                  (finish))))))))
        (_ (user-error "未知选择：%s" choice))))))


(provide 'dida-workflow)
;;; dida-workflow.el ends here
