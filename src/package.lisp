(in-package :cl-user)


(defpackage #:messagepack-rpc.utils
  (:use #:cl)
  ;; utils.lisp
  (:export #:while
           #:symbol-concat
           #:mklst))

(defpackage #:event-loop
  (:nicknames #:el)
  (:use #:cl
        #:bordeaux-threads 
        #:cffi
        #:cl-async
        #:messagepack-rpc.utils)
  (:import-from #:libuv #:uv-run-mode #:uv-run)
  ;; event-loop.lisp
  (:export #:init
           #:add-listener
           #:run-once
           #:run-forever
           #:send
           #:handle-msg
           #:clean)
  ;; future.lisp
  (:export #:future
           #:finishedp
           #:result
           #:finish
           #:join))

(defpackage #:messagepack-rpc
  (:nicknames #:mrpc)
  (:use #:cl
        #:messagepack
        #:flexi-streams
        #:event-loop
        #:alexandria
        #:messagepack-rpc.utils)
  ;; cl-messagepack
  (:export #:*extended-types*)
  ;; session.lisp
  (:export #:session
           #:register-callback
           #:remove-callback
           #:call
           #:call-async
           #:request
           #:notify))
