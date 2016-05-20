(asdf:defsystem #:cl-messagepack-rpc-tests
  :description "Tests for cl-messagepack-rpc"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-messagepack-rpc
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/setup")
               (:file "t/client")
               (:file "t/server"))
  :perform (test-op (op c)
             (funcall (intern (symbol-name :run!) (find-package :fiveam))
                      (intern (symbol-name :messagepack-rpc-test-suite) (find-package :messagepack-rpc-tests)))))
