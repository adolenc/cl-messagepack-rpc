(in-package #:messagepack-rpc)


(defmacro define-rpc-type (type-name type-id &rest components)
  "Define helper functions for constructing, sending, destructing and verifying messagepack-rpc types."
  (let ((make-name (symbol-concat 'make- type-name))
        (send-name (symbol-concat 'send- type-name))
        (parse-name (symbol-concat 'parse- type-name))
        (component-keywords (mapcar #'(lambda (c) (intern (symbol-name c) :keyword)) components))
        (predicate-name (symbol-concat type-name 'p)))
    `(progn
       (defun ,make-name ,components
         ,(format NIL "Encode parameters into a packet ready to be sent as a ~A." type-name)
         (concatenate '(vector (unsigned-byte 8))
                      (mpk:encode (list ,type-id ,@components))))
       (defmethod ,send-name ((session session) ,@components)
         ,(format NIL "Encode parameters into a ~A packet and send it using session object." type-name)
         (send (event-loop session) (socket session) (,make-name ,@components)))
       (defun ,predicate-name (msg)
         ,(format NIL "Predicate to check if msg is ~A." type-name)
         (= (first msg) ,type-id))
       (defun ,parse-name (msg)
         ,(format NIL "Parse msg into a property list, where keys are ~{:~A~^, ~} and values are corresponding values from msg." component-keywords)
         (zip ',component-keywords (rest msg))))))
