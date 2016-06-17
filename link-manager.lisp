;;;; link-manager.lisp

;;;; TODO; figure out order to execute
;;;; for now:
;;;; - lisp --load package.lisp
;;;; - (load "link-manager.lisp")
;;;; - (in-package :link-manager)

;; Sorting:
;; (sort *db* '< :key 'date-added)

(in-package :link-manager)


(defvar *db* nil)
(defvar *web-acceptor* nil)

(defparameter *database* "test.db")
(defparameter *counter* "counter")
(defparameter *web-port* 8080)

; set default counter to a function, then call in on creation of id
(defvar *highest-id* 0)

(defun next-value ()
  #'(lambda () (incf *highest-id*)))

(defvar get-id (next-value))

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
    (make-bookmark :title title :link link :summary summary :tags tags
                   :read? read?)))

(defun select-links-with-tags (tags-list database)
  "Select all the bookmarks with tags"
  (cond
    ((equal (length tags-list) 1) (select-in tags tags-list database))
    ((> (length tags-list) 1)
     (select-links-with-tags (rest tags-list) (select-in tags tags-list database)))
    (t database)))

(defun select-links-with-summary (summary-list database)
  "Select all the bookmarks with summary"
  (cond
    ((equal (length summary-list) 1) (select-in summary summary-list database))
    ((> (length summary-list) 1)
     (select-links-with-summary (rest summary-list) (select-in summary summary-list database)))
    (t database)))

; (select (where 'read? nil) :summary '(lisp))
; (select  :fn (where 'read? nil))
(defun select (&key (fn #'(lambda (x) x)) tags summary)
  "(select :fn (where id 1) :tags '(python) :summary '(language))"
  (select-links-with-summary summary
                             (select-links-with-tags tags
                                                     (remove-if-not fn *db*))))

(defun select-by-id (id)
  (first (select :fn (where 'id id))))

(defun search-bookmarks (items database)
  ()
  )

(defun delete-link (id)
  (setf *db* (remove-if #'(lambda (link) (equal (id link) id)) *db*)))

; * (show-all-unique-elements #'tags *db*)
; * (show-all-unique-elements #'summary *db*)
; (FUNCTIONAL DJANGO LISP HASKELL PYTHON VIM PROGRAMMING EDITOR SEARCH)



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

;; (sort *db* '< :key 'date-added)

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
