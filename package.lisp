;;;; package.lisp

(defpackage :link-manager
  (:use :cl :hunchentoot)
  (:export :make-link
           :load-db
           :start-server
           :stop-server
           :save
           :select
           :where))

