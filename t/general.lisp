(in-package :messagepack-rpc-tests)
(def-suite messagepack-rpc-test-suite :description "Test suite for cl-messagepack-rpc")
(in-suite messagepack-rpc-test-suite)


(defparameter *client* NIL)
(defparameter *connections* '(
                               (:file "/tmp/nvim")
                              ;(:host "127.0.0.1" :port 9900)
                              ;()
                             ))

(def-fixture mrpc-client (connections)
  (dolist (connection connections)
    (let ((*client* (apply #'make-instance 'mrpc:session connection)))
      (mrpc:call *client* "vim_input" "o<esc>")
      (&body))))


(test sample 
  (with-fixture mrpc-client (*connections*)
    (mrpc:call *client* "vim_set_current_line" "hello world!")
    (is (string= "hello world!" (mrpc:call *client* "vim_get_current_line")))))

(test sample2
  (with-fixture mrpc-client (*connections*)
    (mrpc:call *client* "vim_set_current_line" "hello world!")
    (is (string= "hello world!" (mrpc:call *client* "vim_get_current_line")))))

(test stress-test
  (with-fixture mrpc-client (*connections*)
    (dotimes (i 100)
      (mrpc:call *client* "vim_set_current_line" (format NIL "~A" i))
      (is (string= (format NIL "~A" i) (mrpc:call *client* "vim_get_current_line"))))))
