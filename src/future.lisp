(in-package #:messagepack-rpc.event-loop)


(defclass future ()
  ((finished :initarg :finished :accessor finishedp :initform NIL)
   (result :initarg :result :accessor result :initform NIL)
   (err :initarg :error :accessor err :initform NIL)
   (event-loop :initarg :event-loop :accessor event-loop :initform (error "Must specify the event-loop which the object should follow"))))

(defmethod finish ((future future) &key result error)
  "Set FUTURE's status to finished, and set its RESULT and/or ERROR."
  (setf (finishedp future) T
        (result future) result
        (err future) error))

(defmethod join ((future future))
  "Block the execution of current thread until FUTURE has a result from the
server. Then either return a result, or throw an error, depending on how the
server responded."
  (while (not (finishedp future))
    (run-once (event-loop future)))
  (if (err future)
    (error 'rpc-error :message (format NIL "~A" (err future)))
    (result future)))
