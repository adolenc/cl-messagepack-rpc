(in-package #:messagepack-rpc)


(defparameter *max-request-id* 0 "Maximal request id used")

(defun get-unique-request-id ()
  "Return a unique, not yet used request id."
  (incf *max-request-id*))

(defclass session ()
  ((event-loop :initform (el:init) :initarg :event-loop :accessor event-loop)
   (socket :initform NIL :accessor socket)
   (callbacks :initform (make-hash-table :test 'equal) :accessor callbacks)
   (extended-types :initform '() :accessor extended-types)
   (lookup-table :initform (mpk:make-lookup-table) :accessor lookup-table) ; for eq between ext objects
   (active-requests :initform (make-hash-table) :accessor active-requests)))

(define-rpc-type request 0 id method params)
(define-rpc-type response 1 id error result)
(define-rpc-type notification 2 method params)


(defmethod callback-handler ((session session) bytes)
  "Decode messages from BYTES and call ON-MESSAGE with the decoded data. This
method gets registered with SESSION's event-loop as the default callback
handler."
  (let ((mpk:*decoder-prefers-lists* T)
        (mpk:*extended-types* (extended-types session))
        (mpk:*lookup-table* (lookup-table session)))
    ; BYTES may contain multiple messages so we need to properly split it up
    ; and evaluate each one separately.
    (flexi-streams:with-input-from-sequence (stream bytes)
      (while (listen stream)
        (on-message session (mpk:decode-stream stream))))))

(defmethod register-callback ((session session) method callback)
  "Register a CALLBACK which will get called when server/client sends
request or notification for METHOD."
  (setf (gethash method (callbacks session)) callback))

(defmethod remove-callback ((session session) method)
  "Remove a registered callback with name METHOD."
  (remhash method (callbacks session)))


(defmethod call-async ((session session) method &rest params)
  "Use session to call server's METHOD with specified PARAMS and immediately
pass control back to the caller, returning a future object. If you want to
later check the results, use JOIN on the future."
  (let* ((id (get-unique-request-id))
         (future (make-instance 'el:future :event-loop (event-loop session))))
    (setf (gethash id (active-requests session)) future)
    (send-request session id method (or params #()))
    future))

(defmethod call ((session session) method &rest params)
  "Invoke CALL-ASYNC with the specified arguments, and call JOIN on the
returned future. This call thus blocks the thread until response from the
server is received."
  (join (apply #'call-async session method params)))

(defmethod request ((session session) method &rest params)
  "Alias for CALL."
  (apply #'call session method params))

(defmethod notify ((session session) method &rest params)
  "Use SESSION to call server's METHOD with specified PARAMS, immediately
returning control to the caller. This call completely ignores server
responses."
  (send-notification session method (or params #()))
  T)


(defmethod on-message ((session session) message)
  "Properly handle a newly received message by dispatching it to the correct
handler based on its type."
  (cond ((requestp message) (apply #'on-request session (parse-request message)))
        ((responsep message) (apply #'on-response session (parse-response message)))
        ((notificationp message) (apply #'on-notification session (parse-notification message)))))

(defmethod apply-callback ((session session) method params)
  "Find appropriate callback for session and call it with params, or trigger
no-method-error if no method with correct name is registered."
  (let ((callback (gethash method (callbacks session))))
    (if callback
      (apply callback params)
      (error 'no-method-error :message method))))

(defmethod on-request ((session session) &key id method params)
  "Handle a new request from the server, calling the appropriate callback and
responding with its return value if the call is successful, or catch the
error and respond with it."
  (handler-bind ((error #'(lambda (desc)
                         (send-response session id
                                        (format NIL "'~A' method: ~A~%~A"
                                                method desc (print-backtrace desc :output nil))
                                        NIL))))
    (send-response session id NIL (apply-callback session method params))))

(defmethod on-response ((session session) &key id error result)
  "Handle a response from the server by finishing the appropriate future from
active-requests of the session with the received result or error."
  (let ((future (gethash id (active-requests session))))
    (when (gethash id (active-requests session))
      (remhash id (active-requests session))
      (finish future :error error :result result))))

(defmethod on-notification ((session session) &key method params)
  "Handle a new notification from the server by calling the appropriate
 callback."
  (handler-case (apply-callback session method params)
    (error ()))) ; We just ignore any errors when it comes to notifications
