(in-package #:messagepack-rpc.conditions)


(define-condition rpc-error (error)
  ((message :initform "" :initarg :message :accessor message))
  (:report (lambda (c s) (format s "~A" (message c)))) )

(define-condition transport-error (rpc-error)
  ()
  (:report (lambda (c s) (format s "Error with connection: ~A" (message c)))))

(define-condition call-error (rpc-error)
  ())

(define-condition no-method-error (call-error)
  ()
  (:report (lambda (c s) (format s "'~A' method not found." (message c)))))

(define-condition argument-error (call-error)
  ()
  (:report (lambda (c s) (format s "'~A' method not found." (message c)))))
