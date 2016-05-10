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
(defparameter *counter* "counter")

; set default counter to a function, then call in on creation of id
(defvar *highest-id* 0)

(defun next-value (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *highest-id* (read in))))
  #'(lambda () (incf *highest-id*)))

(defvar get-id (next-value *counter*))

; (multiple-value-bind (sec min hour day month year)
; (get-decoded-time) (format nil "~A ~2,'0d~2,'0d ~A ~A" year month day hour s

(defstruct (bookmark :conc-name)
  "Bookmark structure"
  (id (funcall get-id) :read-only t)  ; set default function next-value then call it on creation
  title
  link
  summary
  (tags '() :type list)
  (date-added (get-universal-time) :read-only t)
  (read? nil))

(defun add-record (link)
  "Push cd onto *db* stack"
  (push link *db*))

(defun make-link (title link summary tags &optional read?)
  "Create structure for link"
  (add-record
    (make-bookmark :title title :link link :summary summary :tags tags
                   :read? read?)))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun save-counter (filename)
  "Save current *counter* to file."
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *highest-id* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defmacro where-keyword-in (func attribute database)
  "(remove-if-not #'(lambda (link) (member 'lisp (bookmark-tags link))) *db*)"
  `(remove-if-not #'(lambda (link) (find (first ,attribute) (,func link))) ,database))

(defun select-links-with-tags (tags-list database)
  "Select all the bookmarks with tags"
  (cond
    ((equal (length tags-list) 1) (where-keyword-in tags tags-list database))
    ((> (length tags-list) 1)
     (select-links-with-tags (rest tags-list) (where-keyword-in tags tags-list database)))
    (t database)))

(defun select-links-with-summary (summary-list database)
  "Select all the bookmarks with summary"
  (cond
    ((equal (length summary-list) 1) (where-keyword-in summary summary-list database))
    ((> (length summary-list) 1)
     (select-links-with-summary (rest summary-list) (where-keyword-in summary summary-list database)))
    (t database)))

(defun select (selector-fn &key tags summary)
  "(select (where 'read? ()) :tags '(python) :summary '(language))"
  (select-links-with-summary summary
                             (select-links-with-tags tags
                                                     (remove-if-not selector-fn *db*))))

(defun flatten (l)
  "Flatten list."
  (cond ((null l) nil)
        ((atom (first l)) (cons (first l) (flatten (rest l))))
        (t (append (flatten (first l)) (flatten (rest l))))))

(defun show-all-tags (database)
  "Make a list of all unique tags."
  (remove-duplicates (flatten (mapcar 'tags database))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

(defun make-comparison-exp (field value)
  `(equal (slot-value link ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields collecting
        (make-comparison-exp (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (link) (and ,@(make-comparison-list clauses))))

(defun update (selector-fn &key title link summary tags (read? nil read-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title   (setf (title row) title))
                (if link    (setf (link row) link))
                (if summary (setf (summary row) summary))
                (if tags    (setf (tags row) tags))
                (if read-p  (setf (read? row) read?)))
              row) *db*)))

(defun save ()
  (save-db "test.db")
  (save-counter "counter"))
