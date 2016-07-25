(in-package :link-manager)


(defvar *db* nil)
(defvar *web-acceptor* nil)

; (defvar *successfully-added* nil)

(defparameter *database* "test.db")
(defparameter *counter* "counter")
(defparameter *web-port* 8080)

(defvar *db* nil)
(defvar *web-acceptor* nil)

; (defvar *successfully-added* nil)

(defparameter *database* "test.db")
(defparameter *counter* "counter")
(defparameter *web-port* 8080)

(defparameter *haystacks* '(title summary tags)
  "These are the items you can search in")


(clsql:connect '("localhost" "bookmarks" "postgres" "postgres")
               :database-type :postgresql)
