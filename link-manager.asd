;;;; link-manager.asd
;;;; use cl-who hunchentoot etc.

(asdf:defsystem #:link-manager
  :description "Describe link-manager here"
  :author "Arjen Dijkstra <dijkstra.arjen@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:hunchentoot :cl-who :split-sequence :html-template)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "dispatchers")
               (:file "utils")
               (:file "link-manager")
               (:file "web")))

