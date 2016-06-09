;;; package.lisp
;;;; link-manager.lisp
;;;; depends on Hunchentoot
;;;; TODO; implement search, search for term
;;;; (or (member term title) (member term tags) (member term summary)

(defpackage :link-manager
  (:use :cl :hunchentoot :cl-who))
