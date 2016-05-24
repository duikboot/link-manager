;;;; package.lisp
;;;;

(defpackage :link-manager
  (:use :cl :hunchentoot :cl-who)
  (:export :make-link
           :date-modified
           :select
           :where))
