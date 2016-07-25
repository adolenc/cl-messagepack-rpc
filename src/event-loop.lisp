(in-package #:messagepack-rpc.event-loop)


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
        (as:signal-handler as:+sigterm+ #'(lambda (sig)
                                            (declare (ignore sig))
                                            (clean event-loop "Received sigterm. Event loop exited.")))
        (as:signal-handler as:+sigint+ #'(lambda (sig)
                                           (declare (ignore sig))
                                           (clean event-loop "Received sigint. Event loop exited.")))
        event-loop))))

(defun run-once (event-base)
  "Run event loop once, blocking the execution of the thread until a new
 message is received."
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-once))))

(defun run-forever (event-base)
  "Run event loop forever, blocking the execution and processing all received
 messages."
  (with-event-loop-bindings (event-base)
    (uv:uv-run (as::event-base-c event-base) (cffi:foreign-enum-value 'uv:uv-run-mode :run-default))))

(defun clean (event-base &optional with-error)
  "Clean up and properly close the event-loop, and throw an error with-error if
 it is supplied."
  (with-event-loop-bindings (event-base)
    (as:exit-event-loop)
    (as::do-close-loop (as::event-base-c as::*event-base*))
    ; TODO: properly free in/out buffers
    ; (static-vectors:free-static-vector as::*output-buffer*)
    ; (static-vectors:free-static-vector as::*input-buffer*)
    (as::free-pointer-data (as::event-base-c as::*event-base*) :preserve-pointer t)
    (if with-error (error 'transport-error :message with-error))))

; TODO: in/out buffers should probably not be global, but unique per session
(setf as::*output-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
(setf as::*input-buffer* (static-vectors:make-static-vector as::*buffer-size* :element-type 'octet))
