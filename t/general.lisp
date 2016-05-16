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

(test-client raise-error
  (signals simple-error (mrpc:call *client* "raise_error")))

(test-client unknown-method
  (signals simple-error (mrpc:call *client* "unknown" T))
  (handler-case (progn (mrpc:call *client* "unknown" T)
                       (fail))
    (error (e) (is (string= "'unknown' method not found" (format NIL "~A" e))))))

(test async-result
  (with-fixture mrpc-client (*connections*)
    (is (string= "You are async!" (mrpc:call *client* "async_result")))))
