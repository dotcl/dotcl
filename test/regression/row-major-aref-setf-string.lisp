;;; (SETF ROW-MAJOR-AREF) on a simple string. ROW-MAJOR-AREF itself read
;;; strings, but the setter only knew general vectors and signalled "not a
;;; vector" for a string, which series' collectors run into.

(deftest row-major-aref-setf-simple-string
  (let ((s (make-string 3 :initial-element #\a)))
    (setf (row-major-aref s 1) #\b)
    s)
  "aba")

(deftest row-major-aref-setf-string-returns-value
  (let ((s (copy-seq "xyz")))
    (values (setf (row-major-aref s 2) #\Q) s))
  #\Q "xyQ")

(deftest row-major-aref-setf-string-non-character
  (let ((s (copy-seq "abc")))
    (handler-case (progn (setf (row-major-aref s 0) 1) :no-error)
      (type-error () :type-error)))
  :type-error)

(deftest row-major-aref-setf-string-out-of-range
  (let ((s (copy-seq "abc")))
    (handler-case (progn (setf (row-major-aref s 3) #\a) :no-error)
      (error () :error)))
  :error)
