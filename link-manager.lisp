;;;; link-manager.lisp


;;; * (mapcar #'(lambda(link) (member 'python (bookmark-tags link))) *db*)
;;; * (select #'(lambda(link) (member 'python (bookmark-tags link))))

; (select (and #'(lambda(link) (find 'lisp (bookmark-tags link)))
;              #'(lambda(link) (find 'python (bookmark-tags link)))))

; (or #'(lambda(link) (member 'lisp (bookmark-tags link)))
;     #'(lambda(link) (member 'python (bookmark-tags link))))


; (remove-if-not #'(lambda (link) (member 'lisp (bookmark-tags link))) *db*)

; (remove-if-not #'(lambda (link) (member 'python (bookmark-tags link)))
;                (remove-if-not #'(lambda (link) (member 'lisp (bookmark-tags
;                                                                link))) *db*))

(in-package :link-manager)


(defvar *db* nil)

(defstruct bookmark
  "Bookmark structure"
  title
  link
  (tags '() :type list)
  (date-added (get-decoded-time) :read-only t)
  (read? nil))

(defun add-record (link)
  "Push cd onto *db* stack"
  (push link *db*))

(defun make-link (title link tags &optional read?)
  "Create structure for link"
  (add-record
    (make-bookmark :title title :link link :tags tags :read? read?)))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

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

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-exp (field value)
  `(equal (slot-value link ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields collecting
        (make-comparison-exp (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (link) (and ,@(make-comparison-list clauses))))

(defun update (selector-fn &key title link (read? nil read-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title  (setf (getf row :title) title))
                (if link   (setf (getf row :link) link))
                (if read-p (setf (getf row :read?) read?)))
              row) *db*)))
