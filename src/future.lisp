(in-package #:event-loop)


(defclass future ()
  ((finished :initarg :finished :accessor finishedp :initform NIL)
   (result :initarg :result :accessor result :initform NIL)
   (err :initarg :error :accessor err :initform NIL)
   (event-loop :initarg :event-loop :accessor event-loop :initform (error "Must specify event-loop to use."))))

(defmethod finish ((future future) &key result error)
  (setf (finishedp future) T
        (result future) result
        (err future) error))

(defmethod join ((future future))
  (while (not (finishedp future))
    (run-once (event-loop future)))
  (if (err future)
    (error (err future))
    (result future)))
