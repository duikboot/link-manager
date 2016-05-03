;;;; package.lisp

(defpackage :link-manager
  (:use :cl)
  (:export :make-link
           :save-db
           :load-db
           :select
           :where))

