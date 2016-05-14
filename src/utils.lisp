(in-package #:messagepack-rpc.utils)


(defun symbol-concat (&rest symbols)
  "Concatenate symbol names."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defmacro while (test &rest body)
  "Imitate the standard while loop."
  `(do ()
       ((not ,test))
     ,@body))

(defun zip (&rest lists)
  "Zip lists together."
  (apply #'alexandria:mappend #'list lists))
