(asdf:defsystem #:cl-messagepack-rpc
  :description "Implementation of client side for messagepack-rpc specification."
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-messagepack #:cl-async #:cl-libuv #:flexi-streams)
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/future")
               (:file "src/event-loop")
               (:file "src/messages")
               (:file "src/session")))
