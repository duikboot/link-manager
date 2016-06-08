(in-package :link-manager)

(defvar *log* *error-output*)

(defun l (&rest args)
  (apply #'format *log* args))

(defun flatten (l)
  "Flatten list."
  (cond ((null l) nil)
        ((atom (first l)) (cons (first l) (flatten (rest l))))
        (t (append (flatten (first l)) (flatten (rest l))))))

