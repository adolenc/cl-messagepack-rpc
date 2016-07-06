(in-package :messagepack-rpc-tests)
(in-suite messagepack-rpc-test-suite)


;; We don't have server implemented yet, but we support request/notifcation
;; handlers for client, so let's simulate fake requests and check client's
;; responses.

(defmacro simulate-request ((id method &rest params) &body preparations)
  "Simulate a simple request to *client*, which can be pre-prepared, and return
*client*'s decoded response."
  (let ((in (gensym))
        (out (gensym))
        (response (gensym)))
    `(let* ((,in (flex:make-in-memory-input-stream (mrpc::make-request ,id ,method ',params)))
            (,response (flex:with-output-to-sequence (,out)
                        (let ((*client* (make-instance 'mrpc:client :input-stream ,in :output-stream ,out)))
                          ,@preparations
                          (mrpc::run-once (mrpc::event-loop *client*)))))
            (mpk::*decoder-prefers-lists* T))
       (mrpc::parse-response (mpk:decode ,response)))))

(test on-request
  (let ((sum #'(lambda (a b &rest ns) (apply #'+ a b ns))))
    (is (equal '(:id 0 :error NIL :result 6)
               (simulate-request (0 "add" 1 2 3)
                 (mrpc:register-callback *client* "add" sum))))))

(test on-invalid-request
  (let ((sum #'(lambda (a b &rest ns) (apply #'+ a b ns)))) ; slightly silly sum to simulate argument errors
    (is (equal '(:id 0 :error "'add' method not found" :result NIL)
               (simulate-request (0 "add" 1 2 3))))
    (is (equal '(:id 1 :error "'add' method: invalid number of arguments" :result NIL)
               (simulate-request (1 "add" 1)
                 (mrpc:register-callback *client* "add" sum))))
    (is (equal '(:id 2 :error "'add' method not found" :result NIL)
               (simulate-request (2 "add" 1 2 3)
                 (mrpc:register-callback *client* "add" sum)
                 (mrpc:remove-callback *client* "add"))))))

(test on-error-in-callback
  (let ((throw-error #'(lambda () (error "I am throwing an error!")))
        (throw-warning #'(lambda () (warn "I am throwing a warning!") "DONE")))
    (is (equal '(:id 0 :error "'add' method: I am throwing an error!" :result NIL)
               (simulate-request (0 "add")
                 (mrpc:register-callback *client* "add" throw-error))))  
    (is (equal '(:id 1 :error NIL :result "DONE")
               (simulate-request (1 "add")
                 (mrpc:register-callback *client* "add" throw-warning))))))
