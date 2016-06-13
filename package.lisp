;;;; link-manager.lisp
;;;; TODO; implement search, search for term
;;;; (or (member term title) (member term tags) (member term summary)

;;; * (sb-ext:save-lisp-and-die "bookmarks" :executable t)


(defpackage :link-manager
  (:use :cl :hunchentoot :cl-who))
