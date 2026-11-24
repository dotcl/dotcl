;;; (file-position stream n) on a file input stream forgot a character pushed
;;; back with UNREAD-CHAR: the position did not move and the next READ-CHAR
;;; returned the pushed-back character again. SBCL moves, as a string input
;;; stream here already did. scribble's stream-line-column-harder reads a char,
;;; unreads it, and then seeks.

(defun %fpuc-file ()
  (format nil "~a/file-position-unread.txt" (regression-temp-dir)))

(defun %fpuc-case (setup)
  (let ((p (%fpuc-file)))
    (with-open-file (o p :direction :output :if-exists :supersede)
      (write-string (format nil "Hello, World~%Fare~%") o))
    (with-open-file (s p :direction :input)
      (funcall setup s)
      (file-position s 1)
      (list (file-position s) (read-char s)))))

(deftest file-position-clears-unread-char.read-unread
  (%fpuc-case (lambda (s) (unread-char (read-char s) s)))
  (1 #\e))

(deftest file-position-clears-unread-char.read-read-unread
  (%fpuc-case (lambda (s) (read-char s) (unread-char (read-char s) s)))
  (1 #\e))

(deftest file-position-clears-unread-char.seek-after-unread-to-zero
  (let ((p (%fpuc-file)))
    (with-open-file (s p :direction :input)
      (unread-char (read-char s) s)
      (file-position s 0)
      (list (file-position s) (read-char s))))
  (0 #\H))
