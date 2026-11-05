;;; test-dida-api.el --- dida.el HTTP 传输层离线测试 -*- lexical-binding: t; -*-

;; 用法（在本仓库根目录）：
;;   emacs -Q --batch -L . -L /path/to/plz -l test/test-dida-api.el
;;
;; 重点覆盖默认 transport（plz）的适配层：方法名必须转成 plz 要的小写符号
;; （否则 plz 的 `symbol-name' 报 `wrong-type-argument symbolp'，且 POST body
;; 不会被发送）；HTTP 非 2xx 响应要原样返回给上层做状态映射，只有真正的
;; 网络/curl 错误才应该变成 `dida-api-error' 的 network kind。

(require 'ert)
(require 'dida)

(defun dida-test--plz-returns (status body)
  "构造一个假 plz：直接返回 STATUS/BODY 的 `plz-response'。"
  (lambda (&rest _)
    (make-plz-response :status status :headers nil :body body)))

(defun dida-test--plz-signals (error-symbol data)
  "构造一个假 plz：抛出 ERROR-SYMBOL，附带 DATA。"
  (lambda (&rest _)
    (signal error-symbol data)))

(ert-deftest dida-api-plz-method-normalization ()
  "字符串方法名要转成 plz 认识的小写符号。"
  (dolist (case '(("GET" . get) ("POST" . post) ("DELETE" . delete)
                  ("patch" . patch) (get . get)))
    (let (seen)
      (cl-letf (((symbol-function 'plz)
                 (lambda (method &rest _)
                   (setq seen method)
                   (make-plz-response :status 200 :headers nil :body "{}"))))
        (dida-api--plz (car case) "http://example.invalid/")
        (should (eq seen (cdr case)))))))

(ert-deftest dida-api-plz-http-error-response-propagates ()
  "非 2xx 时 plz 抛 `plz-http-error'，适配层应返回 response 而不是 network 错误。"
  (let ((response (make-plz-response :status 404 :headers nil :body "{}")))
    (cl-letf (((symbol-function 'plz)
               (dida-test--plz-signals
                'plz-http-error
                (list "HTTP error" (make-plz-error :response response)))))
      (let ((returned (dida-api--plz "GET" "http://example.invalid/")))
        (should (plz-response-p returned))
        (should (equal 404 (plz-response-status returned)))))))

(ert-deftest dida-api-plz-network-error-mapped ()
  "真正的 curl 错误要映射为 network kind，并保留 plz 的错误消息。"
  (cl-letf (((symbol-function 'plz)
             (dida-test--plz-signals
              'plz-curl-error
              (list "Curl error"
                    (make-plz-error :curl-error '(7 . "Failed to connect"))))))
    (let ((err (should-error (dida-api--plz "GET" "http://example.invalid/")
                             :type 'dida-api-error)))
      (should (eq (dida-api-error-kind err) 'network))
      (should (string-match-p "Failed to connect"
                              (dida-api-error-message err))))))

(ert-deftest dida-api-request-maps-404-to-not-found ()
  "`dida-api--request' 应把 404 映射为 not-found，而不是 network。"
  (let ((dida-token "fake-token")
        (response (make-plz-response :status 404 :headers nil :body "{}")))
    (cl-letf (((symbol-function 'plz)
               (dida-test--plz-signals
                'plz-http-error
                (list "HTTP error" (make-plz-error :response response)))))
      (let ((err (should-error (dida-api--request "GET" "/missing")
                               :type 'dida-api-error)))
        (should (eq (dida-api-error-kind err) 'not-found))
        (should (equal 404 (dida-api-error-status err)))))))

(ert-deftest dida-api-request-sends-json-body ()
  "POST 的 JSON body 要通过 plz 的 :body 发送。"
  (let ((dida-token "fake-token") sent)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (setq sent args)
                 (make-plz-response :status 200 :headers nil :body "{}"))))
      (dida-api--request "POST" "/task" :body '((title . "你好")))
      (should (equal "{\"title\":\"你好\"}" (plist-get sent :body)))
      (should (eq 'text (plist-get sent :body-type))))))

;;; ---------------- 异步路径 ----------------
;;
;; 注入的假异步 transport 同步地调用 callback（真实 plz 是异步的，测试只
;; 需要验证结果与状态映射）；重试走 `run-at-time'，batch 里用
;; `accept-process-output' pump 定时器。

(defun dida-test--async-response (status body)
  "构造一个总是返回 STATUS/BODY 的假异步 transport。"
  (lambda (_method _url callback &rest _args)
    (funcall callback (make-plz-response :status status :headers nil :body body) nil)))

(defun dida-test--async-error (kind message)
  "构造一个总是回调错误数据的假异步 transport。"
  (lambda (_method _url callback &rest _args)
    (funcall callback nil (dida-api--error-data kind :message message))))

(defun dida-test--pump-until (predicate &optional seconds)
  "在 batch 里 pump 定时器/进程直到 PREDICATE 为真或超时。"
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(ert-deftest dida-api-async-request-parses-body ()
  "异步请求成功时应把解析后的 body 交给 callback。"
  (let* ((dida-token "fake-token")
         (dida-api-async-transport-function
          (dida-test--async-response 200 "{\"title\":\"x\"}"))
         (result nil))
    (dida-api--request-async
     "GET" "/task/T1"
     (lambda (data err) (setq result (cons data err))))
    (should (equal (alist-get 'title (car result)) "x"))
    (should (null (cdr result)))))

(ert-deftest dida-api-async-request-maps-404-to-not-found ()
  "异步请求的 404 要映射为 not-found 错误数据（不是 network）。"
  (let* ((dida-token "fake-token")
         (dida-api-async-transport-function
          (dida-test--async-response 404 "{}"))
         (result nil))
    (dida-api--request-async
     "GET" "/missing"
     (lambda (data err) (setq result (cons data err))))
    (should (null (car result)))
    (should (eq (plist-get (cdr result) :kind) 'not-found))
    (should (equal 404 (plist-get (cdr result) :status)))))

(ert-deftest dida-api-async-network-error-callback ()
  "网络错误要以 network kind 回调，而不是抛异常。"
  (let* ((dida-token "fake-token")
         ;; 关掉重试：这里只验证错误回调本身
         (dida-api-max-retries 0)
         (dida-api-async-transport-function
          (dida-test--async-error 'network "连接失败"))
         (result nil))
    (dida-api--request-async
     "GET" "/anything"
     (lambda (data err) (setq result (cons data err))))
    (should (null (car result)))
    (should (eq (plist-get (cdr result) :kind) 'network))
    (should (string-match-p "连接失败" (plist-get (cdr result) :message)))))

(ert-deftest dida-api-async-network-error-retries-with-timer ()
  "异步网络错误用 run-at-time 退避重试，pump 后应恢复。"
  (let* ((dida-token "fake-token")
         (dida-api-backoff-base 0.01)
         (attempts 0)
         (result nil)
         (dida-api-async-transport-function
          (lambda (_method _url callback &rest _args)
            (cl-incf attempts)
            (if (= attempts 1)
                (funcall callback nil (dida-api--error-data 'network :message "临时故障"))
              (funcall callback
                       (make-plz-response :status 200 :headers nil :body "{\"ok\":1}")
                       nil)))))
    (dida-api--request-async
     "GET" "/flaky"
     (lambda (data err) (setq result (cons data err))))
    ;; 第一次同步失败，重试由定时器安排
    (should (null (car result)))
    (should (dida-test--pump-until (lambda () (car result))))
    (should (equal (car result) '((ok . 1))))
    (should (= attempts 2))))

(ert-deftest dida-api-async-500-retries-with-timer ()
  "异步 429/5xx 也要退避重试（幂等请求）。"
  (let* ((dida-token "fake-token")
         (dida-api-backoff-base 0.01)
         (attempts 0)
         (result nil)
         (dida-api-async-transport-function
          (lambda (_method _url callback &rest _args)
            (cl-incf attempts)
            (funcall callback
                     (make-plz-response
                      :status (if (= attempts 1) 500 200)
                      :headers nil
                      :body (if (= attempts 1) "{}" "{\"ok\":1}"))
                     nil))))
    (dida-api--request-async
     "GET" "/boom"
     (lambda (data err) (setq result (cons data err))))
    (should (null (car result)))
    (should (dida-test--pump-until (lambda () (car result))))
    (should (equal (car result) '((ok . 1))))
    (should (= attempts 2))))

(ert-deftest dida-api-async-401-refreshes-once-and-retries ()
  "401 应异步刷新 token 一次后重试，刷新成功后请求成功。"
  (let* ((dida-token (list :access_token "old" :refresh_token "r"
                           :expires_in 3600 :created_at (float-time)))
         (dida-access-token "")
         (requests 0)
         (refreshes 0)
         (result nil)
         (dida-api-async-transport-function
          (lambda (_method _url callback &rest _args)
            (cl-incf requests)
            (funcall callback
                     (make-plz-response
                      :status (if (= requests 1) 401 200)
                      :headers nil
                      :body (if (= requests 1) "{}" "{\"ok\":1}"))
                     nil))))
    (cl-letf (((symbol-function 'dida-auth--refresh-async)
               (lambda (cb)
                 (cl-incf refreshes)
                 (setq dida-token (list :access_token "new" :refresh_token "r"
                                        :expires_in 3600 :created_at (float-time))
                       dida-access-token "new")
                 (funcall cb t))))
      (dida-api--request-async
       "GET" "/task/T1"
       (lambda (data err) (setq result (cons data err)))))
    (should (= requests 2))
    (should (= refreshes 1))
    (should (equal (car result) '((ok . 1))))))

(ert-deftest dida-api-async-get-task-404-is-nil ()
  "异步资源函数：get-task 的 404 要回调 nil，而不是错误。"
  (let* ((dida-token "fake-token")
         (dida-api-async-transport-function
          (dida-test--async-response 404 "{}"))
         (called 0)
         (result nil))
    (dida-api-get-task-async
     "P1" "T1"
     (lambda (data err)
       (cl-incf called)
       (setq result (cons data err))))
    (should (= called 1))
    (should (null (car result)))
    (should (null (cdr result)))))

(ert-deftest dida-api-async-list-projects-pagination ()
  "异步 list-projects 要递归翻页直到短页，并去重。"
  (let* ((dida-token "fake-token")
         (dida-api-backoff-base 0.01)
         (requests 0)
         (result nil)
         (dida-api-async-transport-function
          (lambda (_method _url callback &rest _args)
            (cl-incf requests)
            (funcall callback
                     (make-plz-response
                      :status 200
                      :headers nil
                      :body (pcase requests
                              (1 "[{\"id\":\"P1\"}]")
                              (2 "[{\"id\":\"P2\"}]")
                              (_ "[]")))
                     nil))))
    (dida-api-list-projects-async
     (lambda (projects err) (setq result (cons projects err)))
     :limit 1)
    (should (null (cdr result)))
    (should (equal (mapcar (lambda (project) (alist-get 'id project))
                           (car result))
                   '("P1" "P2")))
    (should (= requests 3))))

(provide 'test-dida-api)
;;; test-dida-api.el ends here

(when noninteractive
  (ert-run-tests-batch-and-exit))
