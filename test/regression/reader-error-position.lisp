;;; A reader error says where in the text it was found.

(defun %rep-message (string)
  (handler-case (progn (read-from-string string) nil)
    (reader-error (e) (princ-to-string e))))

(deftest reader-error-position.unexpected-close-paren
  (%rep-message ")")
  "Unexpected ')' (line 1, column 1)")



(deftest reader-error-position.bad-dispatch-after-newline
  (let ((m (handler-case (with-input-from-string (s (format nil "~%  #<x>")) (read s))
             (reader-error (e) (princ-to-string e)))))
    (and (search "line 2, column" m) t))
  t)
