(asdf:defsystem #:cl-messagepack-rpc-tests
  :description "Tests for cl-messagepack-rpc"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-messagepack-rpc
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/setup")
               (:file "t/general")
               (:file "t/final")))
