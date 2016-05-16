(in-package :messagepack-rpc-tests)
(def-suite messagepack-rpc-test-suite :description "Test suite for cl-messagepack-rpc")
(in-suite messagepack-rpc-test-suite)


(defparameter *client* NIL)
(defparameter *connections* '(
                              ;(:file "/tmp/rpc-test-server")
                               (:host "127.0.0.1" :port 18800)
                              ;()
                             ))

(def-fixture mrpc-client (connections)
  (dolist (connection connections)
    (let ((*client* (apply #'make-instance 'mrpc:session connection)))
      (&body))))

(defmacro test-client (name &body body)
  `(test ,name
     (with-fixture mrpc-client (*connections*)
       ,@body)))
