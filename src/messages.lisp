(in-package #:cl-messagepack-rpc)


(defmacro define-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-concat 'make- type-name))
        (send-name (symbol-concat 'send- type-name))
        (predicate-name (symbol-concat type-name 'p)))
    `(progn
       (defun ,make-name ,components
         (concatenate '(vector (unsigned-byte 8))
                      (mpk:encode (list ,type-id ,@components))))
       (defmethod ,send-name ((session session) ,@components)
         (send (event-loop session) (socket session) (,make-name ,@components)))
       (defun ,predicate-name (msg)
         (= (first msg) ,type-id)))))
