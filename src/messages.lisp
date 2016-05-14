(in-package #:messagepack-rpc)


(defmacro define-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-concat 'make- type-name))
        (send-name (symbol-concat 'send- type-name))
        (parse-name (symbol-concat 'parse- type-name))
        (component-keywords (mapcar #'(lambda (c) (intern (symbol-name c) :keyword)) components))
        (predicate-name (symbol-concat type-name 'p)))
    `(progn
       (defun ,make-name ,components
         (concatenate '(vector (unsigned-byte 8))
                      (mpk:encode (list ,type-id ,@components))))
       (defmethod ,send-name ((session session) ,@components)
         (send (event-loop session) (socket session) (,make-name ,@components)))
       (defun ,predicate-name (msg)
         (= (first msg) ,type-id))
       (defun ,parse-name (msg)
         (zip ',component-keywords (rest msg))))))
