;;;; TODO; figure out order to execute
;;;; for now:
;;;; - lisp --load package.lisp
;;;; - (load "link-manager.lisp")
;;;; - (ql:quickload :link-manager)
;;;; - (in-package :link-manager)

(in-package :link-manager)

; (setf *catch-errors-p* '())
(setf *show-lisp-backtraces-p* t)

(defvar *db* nil)
(defvar *web-acceptor* nil)

(defparameter *database* "test.db")
(defparameter *counter* "counter")
(defparameter *web-port* 9999)


(defparameter *vlime-instance* nil)

(defun start-vlime ()
 (unless *vlime-instance*
  (vlime:main :port 4006 :backend :vlime-usocket)
  (setf *vlime-instance* "Vlime running")))

; set default counter to a function, then call in on creation of id
(defvar *highest-id* 0)

(defun next-value ()
  #'(lambda () (incf *highest-id*)))

(defvar get-id (next-value))

(defparameter *user* nil)
(defparameter *pass* nil)

(defun get-user ()
  (with-open-file (in "passwd" :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (let ((f (read in)))
         (setf *pass* (second f))
         (setf *user* (first f)))))))

(defstruct (bookmark :conc-name)
  "Bookmark structure"
  ; set default function next-value then call it on creation
  (id (funcall get-id) :read-only t)
  (title '() :type list)
  link
  (summary '() :type list)
  (tags '() :type list)
  (date-added (get-universal-time) :read-only t)
  (date-modified (get-universal-time))
  (read? nil))

(defun add-record (link)
  "Push cd onto *db* stack"
  (push link *db*))

(defun make-link (title link summary tags &optional read?)
  "Create structure for link"
  (add-record
    (make-bookmark :title title
                   :link link
                   :summary summary
                   :tags tags
                   :read? read?)))


(defun single (lst)
  "Return true if lst has only one element in it."
  (and (consp lst) (not (rest lst))))

(defun select-links-with-tags (tags-list database)

 "Select all the bookmarks with tags"
  (cond
    ((single tags-list) (select-in tags tags-list database))
    ((plusp (length tags-list))
     (select-links-with-tags (rest tags-list) (select-in tags tags-list database)))
    (t database)))

; (select  :fn (where 'read? nil))
(defun select (&key (fn #'(lambda (x) x)) tags (database *db*))
  "Usage: (select :fn (where id 1) :tags '(python) :summary '(language))"
  (select-links-with-tags tags (remove-if-not fn database)))

(defun select-by-id (id)
  (first (select :fn (where 'id id))))

(defun delete-link (id)
  (setf *db* (remove-if #'(lambda (link) (equal (id link) id)) *db*)))

(defun update (&key (fn #'(lambda (x) x)) title link summary tags (read? nil read-p))
  "(update :fn (where 'id 1) :tags '(python programming homepage tutorial))"
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall fn row)
                (if title   (setf (title row) title))
                (if link    (setf (link row) link))
                (if summary (setf (summary row) summary))
                (if tags    (setf (tags row) tags))
                (if read-p  (setf (read? row) read?))
                (setf (date-modified row) (get-universal-time)))
              row) *db*)))

(defun load-database (filename)
  "Load database file."
  (with-open-file (in filename :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (setf *highest-id* (read in))
        (setf *db* (read in))))))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *highest-id* out)
      (print *db* out))))

(defun save ()
  (save-db *database*))

(defun load-db ()
  (load-database *database*))
