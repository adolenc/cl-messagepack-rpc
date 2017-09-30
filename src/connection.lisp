(in-package #:messagepack-rpc.event-loop)


(defmacro with-event-loop-bindings ((event-base) &body body)
  "Make cl-async think the event loop was running all along, because it
otherwise sometimes refuses to work."
  `(let ((as::*event-base* ,event-base)
         (as::*data-registry* (as::event-base-data-registry ,event-base))
         (as::*function-registry* (as::event-base-function-registry ,event-base)))
     ,@body))

(defun on-connection-close (ev)
  (declare (ignore ev))
  (clean as::*event-base* "Connection closed!"))


(defvar *default-pooling-rate* 0.001 "Elapsed time in seconds between two consecutive reads for connect-stream")

(defun connect-stream (event-base callback &key (input-stream *standard-input*) (output-stream *standard-output*) (pooling-rate *default-pooling-rate*))
  "Establish a connection on top of event-base via input and output streams,
calling CALLBACK with new data when it is available."
  (labels ((collect-input ()
             "Block thread until data is available on input-stream and retrieve it."
             (loop until (listen input-stream)
                   do (sleep pooling-rate)
                   finally (return (loop while (listen input-stream)
                                         collect (read-byte input-stream))))))
    (with-event-loop-bindings (event-base)
      (let* ((result nil)
             (new-msg-ready (as:make-notifier (lambda ()
                                                (funcall callback result))
                                              :single-shot NIL)))
        (bt:make-thread (lambda ()
                          (loop do (progn
                                     (setf result (collect-input))
                                     (as:trigger-notifier new-msg-ready))))))))
  output-stream)

(defun connect-pipe (event-base callback &key file)
  "Establish a connection on top of event-base via named pipe, calling CALLBACK
with new data when it is available."
  (with-event-loop-bindings (event-base)
    (as:pipe-connect file #'(lambda (s d) (funcall callback d)) :event-cb #'on-connection-close)))

(defun connect-tcp (event-base callback &key host port)
  "Establish a connection on top of event-base via TCP, calling CALLBACK with
new data when it is available."
  (with-event-loop-bindings (event-base)
    (as:tcp-connect host port #'(lambda (s d) (funcall callback d)) :event-cb #'on-connection-close)))


(defgeneric send (event-base socket bytes)
  (:documentation "Send BYTES via SOCKET using EVENT-BASE event-loop"))

(defmethod send ((event-base as::event-base) (socket as::socket) msg)
  ; Sending via cl-async's socket - i.e. pipe or tcp
  (with-event-loop-bindings (event-base)
    (as:write-socket-data socket msg :force t)))

(defmethod send ((event-base as::event-base) (socket T) msg)
  ; Sending via custom streams
  (with-event-loop-bindings (event-base)
    (loop for b across msg do (write-byte b socket) finally (force-output socket))))
