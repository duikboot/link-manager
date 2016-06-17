(in-package :link-manager)

(defvar *log* *error-output*)

(defun l (&rest args)
  (apply #'format *log* args))

(defun p (o)
  (format *log* "~a~%" o) o)

(defun flatten (l)
  "Flatten list."
  (cond ((null l) nil)
        ((atom (first l)) (cons (first l) (flatten (rest l))))
        (t (append (flatten (first l)) (flatten (rest l))))))

(defun format-time (str date-time)
  (multiple-value-bind (sec minute hour date month year day daylight-p zone)
    (decode-universal-time date-time)
    (declare (ignore day daylight-p zone))
    (format nil "~a ~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" str year month date hour minute sec)))

(defun create-query-sequence (sequence char)
  (when sequence
    (mapcar #'intern (mapcar #'string-upcase (split-sequence:split-sequence char sequence)))))

(defun show-all-unique-elements (fn database)
  "Make a list of all unique tags.
  (show-all-unique-elements 'tags *db*)"
  (remove-duplicates (flatten (mapcar fn database))))

(defun number-of-occurrences (fn database)
  (let ((elements (mapcar #'(lambda (x) (list x 0)) (show-all-unique-elements fn database)))
        (items (flatten (mapcar fn database))))
    (mapcar #'(lambda(x) (incf (second (assoc x elements))) ) items)
    (sort elements  #'> :key #'second )))



