(in-package #:messagepack-rpc)


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
           (flexi-streams:with-input-from-sequence (stream data)
             (while (listen stream)
               (on-message session (mpk:decode-stream stream))))))
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

(defmethod call-async ((session session) method &rest params)
  (let* ((request-id (get-unique-request-id))
        (future (make-instance 'el:future :event-loop (event-loop session) :id request-id)))
    (format t "Sending request ~A for ~A~%" request-id method)
    (setf (gethash request-id (active-requests session)) future)
    (send-request session request-id method (or params #()))
    future))

(defmethod call ((session session) method &rest params)
  (join (apply #'call-async session method params)))

(defmethod request ((session session) method &rest params)
  (apply #'call session method params))

(defmethod notify ((session session) method &rest params)
  (send-notification session method (or params #()))
  T)

(defmethod on-message ((session session) message)
  (format t "RECEIVED ~A, id=~A~%" message (elt message 1))
  (let* ((request-id (elt message 1))
         (future (gethash request-id (active-requests session))))
    (remhash request-id (active-requests session))
    (finish future :result T)))
