(in-package #:cl-messagepack-rpc.utils)


(defun symbol-concat (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defun mklst (obj)
  (if (listp obj) obj (list obj)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))
