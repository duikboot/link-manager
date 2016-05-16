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
(defparameter *counter* "counter")
(defparameter *web-port* 8080)

(defclass web-acceptor (hunchentoot:acceptor) ())

; set default counter to a function, then call in on creation of id
(defvar *highest-id* 0)

(defun next-value ()
  #'(lambda () (incf *highest-id*)))

(defvar get-id (next-value))

; (multiple-value-bind (sec min hour day month year)
; (get-decoded-time) (format nil "~A ~2,'0d~2,'0d ~A ~A" year month day hour s

(defstruct (bookmark :conc-name)
  "Bookmark structure"
  ; set default function next-value then call it on creation
  (id (funcall get-id) :read-only t)
  title
  link
  (summary '() :type list)
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

(defmacro select-in (func attribute database)
  "(remove-if-not #'(lambda (link) (member 'lisp (bookmark-tags link))) *db*)"
  `(remove-if-not #'(lambda (link)
                      (find (first ,attribute) (,func link))) ,database))

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
; (select  :selector-fn (where 'read? nil))
(defun select (&key (fn #'(lambda (x) x)) tags summary)
  "(select (where 'read? ()) :tags '(python) :summary '(language))"
  (select-links-with-summary summary
                             (select-links-with-tags tags
                                                     (remove-if-not fn *db*))))

(defun delete-link (id)
  (setf *db* (remove-if #'(lambda (link) (equal (id link) id)) *db*)))

(defun flatten (l)
  "Flatten list."
  (cond ((null l) nil)
        ((atom (first l)) (cons (first l) (flatten (rest l))))
        (t (append (flatten (first l)) (flatten (rest l))))))

; * (show-all-unique-elements #'tags *db*)
; * (show-all-unique-elements #'summary *db*)
; (FUNCTIONAL DJANGO LISP HASKELL PYTHON VIM PROGRAMMING EDITOR SEARCH)

(defun show-all-unique-elements (fn database)
  "Make a list of all unique tags."
  (remove-duplicates (flatten (mapcar fn database))))

(defun make-comparison-exp (field value)
  `(equal (slot-value link ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields collecting
        (make-comparison-exp (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (link) (and ,@(make-comparison-list clauses))))

(defun update (&key (fn #'(lambda (x) x)) title link summary tags (read? nil read-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall fn row)
                (if title   (setf (title row) title))
                (if link    (setf (link row) link))
                (if summary (setf (summary row) summary))
                (if tags    (setf (tags row) tags))
                (if read-p  (setf (read? row) read?)))
              row) *db*)))


; (defclass link ()
;     ((id :accessor link-id :initform 0)
;      (title :accessor link-title)
;      (tags :accessor link-tags :initform '())))

; (sort *db* '< :key 'date-added)

(defun load-database (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *highest-id* (read in))
      (setf *db* (read in)))))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
        (print *highest-id* out)
        (print *db* out))))

(defun save ()
  (save-db "test.db"))

(defun load-db ()
  (load-database "test.db"))

(defun start-server ()
  (setf *web-acceptor*
        (hunchentoot:start
          (make-instance 'hunchentoot:easy-acceptor :port *web-port*))))

(defun stop-web-acceptor ()
  (hunchentoot:stop *web-acceptor*))

; (defun start-server ()
;   (when *web-acceptor*
;     (print "Server already started. Restarting")
;     (hunchentoot:stop *web-acceptor*))
;   (print "starting story server on port ~S" *web-port*)
;   (reset-server)
;   (setf *web-acceptor*
;         (make-instance 'web-acceptor
;                        :port *web-port*
;                        :access-log-destination sb-sys:*stdout*
;                        :message-log-destination sb-sys:*stdout*))
;   (hunchentoot:start *web-acceptor*))
