;;;; package.lisp

(defpackage :link-manager
  (:use :cl :hunchentoot)
  (:export :make-link
           :save-db
           :load-db
           :save
           :select
           :where))

