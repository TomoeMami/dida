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

(provide 'test-dida-api)
;;; test-dida-api.el ends here

(when noninteractive
  (ert-run-tests-batch-and-exit))
