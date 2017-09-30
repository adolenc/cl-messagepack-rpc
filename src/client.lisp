(in-package #:messagepack-rpc)


(defclass client (session)
  ())

(defmethod initialize-instance :after ((client client) &key (extended-types '())
                                                       host port
                                                       file
                                                       (input-stream *standard-input*) (output-stream *standard-output*) (pooling-rate el::*default-pooling-rate*))
  (let ((callback #'(lambda (data) (callback-handler client data)))
        (event-loop (event-loop client)))
    (setf (socket client) (cond (file            (el:connect-pipe event-loop callback :file file))
                                ((and host port) (el:connect-tcp  event-loop callback :host host :port port))
                                (t               (el:connect-stream event-loop callback :input-stream input-stream :output-stream output-stream :pooling-rate pooling-rate)))
          (extended-types client) (mpk:define-extension-types extended-types))
    ; Try establishing a connection for the first time when using cl-async's sockets
    (when (subtypep (type-of (socket client)) 'as::socket)
      (el:run-once (event-loop client)))))
