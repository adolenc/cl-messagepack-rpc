(in-package #:messagepack-rpc.event-loop)


(defmacro with-event-loop-bindings ((event-base) &body body)
  "Make cl-async think the event loop was running all along, because it
otherwise sometimes refuses to work."
  `(let ((as::*event-base* ,event-base)
         (as::*data-registry* (as::event-base-data-registry ,event-base))
         (as::*function-registry* (as::event-base-function-registry ,event-base)))
     ,@body))

(defun init ()
  "Initialize a new event-loop and return it."
  (let ((loop-pointer (cffi:foreign-alloc :unsigned-char :count (uv:uv-loop-size))))
    (uv:uv-loop-init loop-pointer)
    (let ((event-loop (make-instance
                       'as::event-base
                       :c loop-pointer
                       :id 0  ; TODO: should id change for each new loop?
                       :catch-app-errors NIL
                       :send-errors-to-eventcb T)))
      (with-event-loop-bindings (event-loop)
        (as:signal-handler 2 #'(lambda (sig)
                                 (declare (ignore sig))
                                 (clean event-loop "Event loop exited.")))
        event-loop))))

(defun add-listener (event-base callback &key host port file)
  "Add a new listener to event-loop."
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
  "Run event loop once, blocking the execution of the thread until a new message is received."
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-once))))

(defun run-forever (event-base)
  "Run event loop forever, blocking the execution and processing all received messages."
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-default))))

(defun send (event-base socket msg)
  "Send msg using event-loop via the provided socket."
  (with-event-loop-bindings (event-base)
    (as:write-socket-data socket msg :force t)))

(defun clean (event-base &optional with-error)
  "Clean up and properly close the event-loop, and throw an error with-error if it is supplied."
  (with-event-loop-bindings (event-base)
    (as:exit-event-loop)
    (as::do-close-loop (as::event-base-c as::*event-base*))
    ; (static-vectors:free-static-vector as::*output-buffer*)
    ; (static-vectors:free-static-vector as::*input-buffer*)
    (as::free-pointer-data (as::event-base-c as::*event-base*) :preserve-pointer t)
    (if with-error (error with-error))))

(setf as::*output-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
(setf as::*input-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
