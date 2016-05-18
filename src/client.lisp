(in-package #:messagepack-rpc)


(defclass client (session)
  ())

(defmethod initialize-instance :after ((client client) &key host port file)
  (setf (socket client) (el:add-listener (event-loop client)
                                         #'(lambda (data) (callback-handler client data))
                                         :host host :port port :file file)))
