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
           (let ((mpk:*decoder-prefers-lists* T))
             (flexi-streams:with-input-from-sequence (stream data)
               (while (listen stream)
                 (on-message session (mpk:decode-stream stream)))))))
    (setf (socket session) (el:add-listener (event-loop session) #'callback-handler :host host :port port :file file))))

(define-rpc-type request 0 id method params)
(define-rpc-type response 1 id error result)
(define-rpc-type notification 2 method params)


(defmethod register-callback ((session session) method callback)
  "Register a callback which will get called when server sends
   request or notification for method."
  (setf (gethash method (callbacks session)) callback))

(defmethod remove-callback ((session session) method)
  "Remove a registered callback."
  (remhash method (callbacks session)))


(defmethod call-async ((session session) method &rest params)
  (let* ((id (get-unique-request-id))
         (future (make-instance 'el:future :event-loop (event-loop session))))
    (setf (gethash id (active-requests session)) future)
    (send-request session id method (or params #()))
    future))

(defmethod call ((session session) method &rest params)
  (join (apply #'call-async session method params)))

(defmethod request ((session session) method &rest params)
  (apply #'call session method params))

(defmethod notify ((session session) method &rest params)
  (send-notification session method (or params #()))
  T)


(defmethod on-message ((session session) message)
  (cond ((requestp message) (apply #'on-request session (parse-request message)))
        ((responsep message) (apply #'on-response session (parse-response message)))
        ((notificationp message) (apply #'on-notification session (parse-notification message)))))

(defmethod on-request ((session session) &key id method params)
  (handler-case (send-response session id NIL (apply (gethash method (callbacks session)) params))
    (error (desc) (send-response session id (format NIL "~A" desc) NIL))))

(defmethod on-response ((session session) &key id error result)
  (let ((future (gethash id (active-requests session))))
    (remhash id (active-requests session))
    (finish future :error error :result result)))

(defmethod on-notification ((session session) &key method params)
  (apply (gethash method (callbacks session)) params))
