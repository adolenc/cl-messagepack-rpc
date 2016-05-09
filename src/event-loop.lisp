(in-package #:event-loop)


; $ ncat -l 9090 -k -c 'xargs -n1 echo'
; * (ql:quickload :cl-messagepack-rpc)
; * (defparameter *loop* (el:init))
; * (defparameter *socket* (el:add-listener *loop* #'el:handle-msg :host "127.0.0.1" :port 9090))
; * (el:send *loop* *socket* (format nil "wow~c" #\newline))

; (as:with-event-loop ()
;   (let ((sock (as:tcp-connect "127.0.0.1" 9090 #'handle-new-msg :data (format nil "GET /~c~c" #\return #\newline))))
;     (as:delay (lambda () (as:write-socket-data sock (format nil "whoa~c" #\newline) :force T)) :time 3)))

(defmacro with-event-loop-bindings ((event-base) &body body)
  `(let ((as::*event-base* ,event-base)
         (as::*output-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
         (as::*input-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
         (as::*data-registry* (as::event-base-data-registry ,event-base))
         (as::*function-registry* (as::event-base-function-registry ,event-base)))
     ,@body))

(defun init ()
  (let ((loop-pointer (cffi:foreign-alloc :unsigned-char :count (uv:uv-loop-size))))
    (uv:uv-loop-init loop-pointer)
    (let ((event-loop (make-instance
                       'as::event-base
                       :c loop-pointer
                       :id 0
                       :catch-app-errors NIL
                       :send-errors-to-eventcb T)))
      (with-event-loop-bindings (event-loop)
        (as:signal-handler 2 #'(lambda (sig)
                                 (declare (ignore sig))
                                 (clean event-loop "Event loop exited.")))
        event-loop))))

(defun add-listener (event-base callback &key host port file)
  (flet ((clean-callback (socket data)
           (declare (ignore socket))
           (funcall callback data))
         (on-connection-close (ev)
           (declare (ignore ev))
           (clean event-base "Connection closed!")))
  (with-event-loop-bindings (event-base)
    (if (or host port file)
      (cond (file            (as:pipe-connect file #'clean-callback :event-cb #'on-connection-close))
            ((and host port) (as:tcp-connect host port #'clean-callback :event-cb #'on-connection-close))
            (t (error "You must specify both host and port.")))
      (run-io-listener)))))

(defun run-once (event-base)
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-once))))

(defun run-forever (event-base)
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-default))))

(defun send (event-base socket msg)
  (with-event-loop-bindings (event-base)
    (as:write-socket-data socket msg :force t)))

(defun clean (event-base &optional with-error)
  (with-event-loop-bindings (event-base)
    (as:exit-event-loop)
    (as::do-close-loop (as::event-base-c as::*event-base*))
    ; (static-vectors:free-static-vector as::*output-buffer*)
    ; (static-vectors:free-static-vector as::*input-buffer*)
    (as::free-pointer-data (as::event-base-c as::*event-base*) :preserve-pointer t)
    (if with-error (error with-error))))
