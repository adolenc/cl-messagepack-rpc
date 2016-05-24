(in-package :cl-user)

(defpackage #:messagepack-rpc.utils
  (:use #:cl)
  ;; utils.lisp
  (:export #:while
           #:symbol-concat
           #:zip))

(defpackage #:messagepack-rpc.conditions
  (:use #:cl)
  ;; conditions.lisp
  (:export #:rpc-error
           #:transport-error
           #:call-error
           #:no-method-error
           #:argument-error))

(defpackage #:messagepack-rpc.event-loop
  (:nicknames #:el)
  (:use #:cl
        #:bordeaux-threads 
        #:cffi
        #:cl-async
        #:messagepack-rpc.utils
        #:messagepack-rpc.conditions)
  (:import-from #:libuv #:uv-run-mode #:uv-run)
  ;; connection.lisp
  (:export #:connect-stream
           #:connect-pipe
           #:connect-tcp
           #:send)
  ;; event-loop.lisp
  (:export #:init
           #:run-once
           #:run-forever
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
        #:alexandria
        #:messagepack-rpc.utils
        #:messagepack-rpc.conditions
        #:messagepack-rpc.event-loop)
  ;; cl-messagepack
  (:export #:define-extension-types
           #:*extended-types*)
  ;; conditions
  (:export #:rpc-error
           #:transport-error
           #:call-error
           #:no-method-error
           #:argument-error)
  ;; event-loop
  (:export #:join)
  ;; session.lisp
  (:export #:register-callback
           #:remove-callback
           #:call
           #:call-async
           #:request
           #:notify)
  ;; client.lisp
  (:export #:client))
