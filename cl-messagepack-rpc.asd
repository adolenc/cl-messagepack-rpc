(asdf:defsystem #:cl-messagepack-rpc
  :description "A Common Lisp implementation of the MessagePack-RPC specification, which uses MessagePack serialization format to achieve efficient remote procedure calls (RPCs)."
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-messagepack
               #:cl-async
               #:cl-libuv
               #:cffi
               #:flexi-streams
               #:trivial-backtrace
               #:alexandria)
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/conditions")
               (:file "src/future")
               (:file "src/connection")
               (:file "src/event-loop")
               (:file "src/messages")
               (:file "src/session")
               (:file "src/client"))
  :in-order-to ((test-op (test-op cl-messagepack-rpc-tests))))
