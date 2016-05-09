(in-package #:cl-messagepack-rpc)


(defparameter *max-request-id* 0)

(defun get-unique-request-id ()
  (incf *max-request-id*))

(defclass session ()
  ((event-loop :initform (el:init) :initarg :event-loop :accessor event-loop)
   (socket :initform NIL :accessor socket)
   (callbacks :initform (make-hash-table :test 'equal) :accessor callbacks)
   (active-requests :initform (make-hash-table) :accessor active-requests)))

(defmethod initialize-instance :after ((session session) &key host port file)
  (flet ((callback-handler (data)
           (on-message session (mpk:decode data))))
    (setf (socket session) (el:add-listener (event-loop session) #'callback-handler :host host :port port :file file))))

(define-rpc-type request 0 msg-id msg-method msg-params)
(define-rpc-type response 1 msg-id msg-error msg-result)
(define-rpc-type notification 2 msg-method msg-params)


(defmethod register-callback ((session session) method callback)
  "Register a callback which will get called when server sends
   request or notification for method."
  (setf (gethash method (callbacks session)) callback))

(defmethod remove-callback ((session session) method)
  "Remove a registered callback."
  (remhash method (callbacks session)))

(defmethod request ((session session) method &rest params)
  (let ((future (make-instance 'el:future :event-loop (event-loop session)))
        (request-id (get-unique-request-id)))
    (setf (gethash request-id (active-requests session)) future)
    (send-request session request-id method (or params #()))
    (prog1 (join future)
      (remhash request-id (active-requests session)))))

(defmethod notify ((session session) method &rest params)
  (send-notification session method (or params #())))

(defmethod on-message ((session session) message)
  (format t "RECEIVED ~A, id=~A~%" message (elt message 1))
  (finish (gethash (elt message 1) (active-requests session)) :result T))
