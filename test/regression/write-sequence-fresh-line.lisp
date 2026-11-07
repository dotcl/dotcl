;;; WRITE-SEQUENCE keeps track of the line position, so ~& and FRESH-LINE after it know whether a newline is due.

(defun %wsf (writer)
  (let ((s (make-string-output-stream)))
    (funcall writer s)
    (format s "~&X")
    (get-output-stream-string s)))

(deftest write-sequence-fresh-line.string
  (list (%wsf (lambda (s) (write-sequence "cdef" s)))
        (%wsf (lambda (s) (write-sequence "cdef" s :start 1)))
        (%wsf (lambda (s) (write-sequence (list #\a #\b) s)))
        (%wsf (lambda (s) (write-sequence (vector #\a #\b) s))))
  ("cdef
X" "def
X" "ab
X" "ab
X"))

(deftest write-sequence-fresh-line.ends-with-newline
  (list (%wsf (lambda (s) (write-sequence (format nil "cd~%") s)))
        (%wsf (lambda (s) (write-sequence "" s)))
        (%wsf (lambda (s) (write-sequence "abc" s :start 3))))
  ("cd
X" "X" "X"))

(deftest write-sequence-fresh-line.output-to-string
  (with-output-to-string (o)
    (write-sequence "ab" o)
    (fresh-line o)
    (write-sequence (format nil "cd~%") o)
    (fresh-line o)
    (write-char #\e o))
  "ab
cd
e")
