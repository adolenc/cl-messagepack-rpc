(in-package :messagepack-rpc-tests)
(in-suite messagepack-rpc-test-suite)


(test-client call
  (is (string= "world" (mrpc:call *client* "hello")))
  (is (= 3 (mrpc:call *client* "sum" 1 2)))  
  (is (null (mrpc:call *client* "nil"))))

(test-client call-async
  (let ((future1 (mrpc:call-async *client* "hello"))
        (future2 (mrpc:call-async *client* "sum" 1 2))
        (future3 (mrpc:call-async *client* "nil")))
    (is (string= "world" (mrpc:join future1)))
    (is (= 3 (mrpc:join future2)))
    (is (null (mrpc:join future3)))))

(test-client notify
  (mrpc:call *client* "clear_register")
  (mrpc:notify *client* "push_to_register" 1)
  (mrpc:notify *client* "push_to_register" "test")
  (mrpc:notify *client* "push_to_register" NIL)
  (is (equal '(1 "test" NIL T) (mrpc:call *client* "push_to_register" T))))

(test ext-type-equality
  (let ((*client* (apply #'make-instance 'mrpc:client
                         (append (first *connections*)
                                 (list :extended-types '(1 ExtClass1 ExtClass2))))))
    (is (eq (mrpc:call *client* "get_ext_type" 1 0)
            (mrpc:call *client* "get_ext_type" 1 0)))
    (is (eq (mrpc:call *client* "get_ext_type" 1 1)
            (mrpc:call *client* "get_ext_type" 1 1)))
    (is (eq (mrpc:call *client* "get_ext_type" 2 0)
            (mrpc:call *client* "get_ext_type" 2 0)))
    (is (eq (mrpc:call *client* "get_ext_type" 2 1)
            (mrpc:call *client* "get_ext_type" 2 1)))
    (is (not (eq (mrpc:call *client* "get_ext_type" 1 0)
                 (mrpc:call *client* "get_ext_type" 1 1))))
    (is (not (eq (mrpc:call *client* "get_ext_type" 2 0)
                 (mrpc:call *client* "get_ext_type" 2 1))))
    (is (not (eq (mrpc:call *client* "get_ext_type" 1 0)
                 (mrpc:call *client* "get_ext_type" 2 0))))
    (let ((ext1 (mrpc:call *client* "get_ext_type" 1 0)))
      (is (eq ext1 (mrpc:call *client* "id" ext1))))))

(test-client raise-error
  (signals mrpc:rpc-error (mrpc:call *client* "raise_error")))

(test-client unknown-method
  (signals mrpc:rpc-error (mrpc:call *client* "unknown" T))
  (handler-case (progn (mrpc:call *client* "unknown" T)
                       (fail))
    (mrpc:rpc-error (e) (is (string= "'unknown' method not found" (format NIL "~A" e))))))

(test-client async-result
  (is (string= "You are async!" (mrpc:call *client* "async_result"))))

(defun random-element (list)
  "Return random element from the list."
  (nth (random (length list)) list))

(test-client stress-test
  (let ((futures (list)))
    (dotimes (i 2000)
      (when (zerop (mod i 100)) (format t "~& Running test ~5<~A~>/2000 " i) (force-output))
      (case (random 6)
        (0 (is (= i (mrpc:call *client* "sum" 0 i))))
        (1 (is (= i (mrpc:request *client* "sum" 0 i))))
        (2 (mrpc:notify *client* "sum" 0 i) (pass))
        (3 (push (list (mrpc:call-async *client* "sum" 0 i) i) futures) (pass))
        (4 (if futures
             (destructuring-bind (future expected-result) (random-element futures)
               (is (= expected-result (mrpc:join future))))
             (pass)))
        (5 (signals mrpc:rpc-error (mrpc:call *client* "raise_error")))))))
