;;;; link-manager.asd
;;;; use cl-who hunchentoot etc.

(asdf:defsystem #:link-manager
  :description "Describe link-manager here"
  :author "Arjen Dijkstra <dijkstra.arjen@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:hunchentoot :cl-who :split-sequence :clsql)
  :serial t
  :components ((:file "package")
               ; (:file "macros")
               (:file "config")
               (:file "models")
               ; (:file "dispatchers")
               ; (:file "utils")
               ; (:file "link-manager")
               ; (:file "web")
               ))
