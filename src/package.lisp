(in-package :cl-user)


(defpackage #:cl-messagepack-rpc.utils
  (:use #:cl)
  (:export #:while
           #:symbol-append
           #:mklst))

(defpackage #:event-loop
  (:nicknames #:el)
  (:use #:cl
        #:bordeaux-threads 
        #:cffi
        #:cl-async
        #:cl-messagepack-rpc.utils)
  (:import-from #:libuv #:uv-run-mode #:uv-run)
  ;; event-loop.lisp
  (:export #:run-forever
           #:run-once
           #:init
           #:clean
           #:add-listener
           #:handle-msg
           #:send)
  ;; future.lisp
  (:export #:finishedp
           #:result
           #:finish
           #:join))
