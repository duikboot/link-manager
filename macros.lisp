(in-package :link-manager)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-comparison-exp (field value)
    `(equal (slot-value link ,field) ,value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-comparison-list (fields)
   (loop while fields collecting
         (make-comparison-exp (pop fields) (pop fields)))))

(defmacro where (&rest clauses)
  `#'(lambda (link) (and ,@(make-comparison-list clauses))))

(defmacro select-in (func attribute database)
  "(remove-if-not #'(lambda (link) (member 'lisp (bookmark-tags link))) *db*)"
  `(remove-if-not #'(lambda (link)
                      (find (first ,attribute) (,func link))) ,database))
