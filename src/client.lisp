(in-package #:messagepack-rpc)


(defclass client (session)
  ())

(defmethod initialize-instance :after ((client client) &rest args &key host port file input-stream output-stream)
  (let ((connection-type (cond (file #'el:connect-pipe)
                               ((and host port) #'el:connect-tcp)
                               (t #'el:connect-stream))))
    (setf (socket client) (apply connection-type
                                 (event-loop client)
                                 #'(lambda (data) (callback-handler client data))
                                 args))
    ; Try establishing a connection for the first time when using cl-async's sockets
    (when (subtypep (type-of (socket client)) 'as::socket)
      (el:run-once (event-loop client)))))
