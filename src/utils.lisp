(in-package #:cl-messagepack-rpc.utils)


(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defun mklst (obj)
  (if (listp obj) obj (list obj)))

(defun keep-lsts-with (kwd lst)
  (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))

(defun remove-lsts-with (kwd lst)
  (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))

(defun first-els (lst)
  (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst))) 

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

