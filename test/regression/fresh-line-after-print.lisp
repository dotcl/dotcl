;;; FRESH-LINE after PRINT / PRIN1 / PRINC / WRITE / PPRINT starts a new line.
;;;
;;; Bug: FRESH-LINE decides from a per-stream "at line start" flag, and only
;;; WRITE-CHAR / WRITE-STRING / TERPRI / FORMAT kept it current. The printer
;;; entry points wrote their text straight to the stream's writer and left the
;;; flag as it was, so right after a newline (PRINT starts with one) the flag
;;; still said "line start" and (princ 1) (fresh-line) (princ 2) printed "12".
;;; FRESH-LINE itself also did not record the newline it wrote.
;;;
;;; A string output stream is not a witness: FRESH-LINE looks at its contents
;;; instead of the flag. A file stream uses the flag, so that is what is tested.

(defparameter *flp-dir* (regression-temp-dir))

(defun %flp-run (fn)
  "Call FN with *STANDARD-OUTPUT* and its argument bound to a fresh file
stream, and return the file's contents."
  (let ((path (format nil "~a/dotcl-fresh-line-~d.txt" *flp-dir* (random 1000000000))))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede)
             (let ((*standard-output* s))
               (funcall fn s)))
           (with-open-file (s path)
             (let ((out (make-string-output-stream)))
               (loop for c = (read-char s nil) while c do (write-char c out))
               (get-output-stream-string out))))
      (ignore-errors (delete-file path)))))

(defun %flp-nl (&rest parts)
  (format nil "~{~a~^~%~}" parts))

(deftest fresh-line-after-print.princ-default
  (%flp-run (lambda (s) (declare (ignore s)) (princ 1) (fresh-line) (princ "b")))
  #.(%flp-nl "1" "b"))

(deftest fresh-line-after-print.print-default
  (%flp-run (lambda (s) (declare (ignore s)) (print 1) (fresh-line) (princ "b")))
  #.(%flp-nl "" "1 " "b"))

(deftest fresh-line-after-print.prin1-stream
  (%flp-run (lambda (s) (terpri s) (prin1 "x" s) (fresh-line s) (princ "b" s)))
  #.(%flp-nl "" "\"x\"" "b"))

(deftest fresh-line-after-print.print-stream
  (%flp-run (lambda (s) (print 2 s) (fresh-line s) (princ "b" s)))
  #.(%flp-nl "" "2 " "b"))

(deftest fresh-line-after-print.princ-stream
  (%flp-run (lambda (s) (terpri s) (princ 3 s) (fresh-line s) (princ "b" s)))
  #.(%flp-nl "" "3" "b"))

(deftest fresh-line-after-print.write
  (%flp-run (lambda (s) (terpri s) (write 7 :stream s :base 2) (fresh-line s)
              (write 8) (fresh-line) (princ "b" s)))
  #.(%flp-nl "" "111" "8" "b"))

(deftest fresh-line-after-print.pprint
  (%flp-run (lambda (s) (pprint '(a b) s) (fresh-line s) (princ "b" s)))
  #.(%flp-nl "" "(A B)" "b"))

(deftest fresh-line-after-print.synonym-stream
  (%flp-run (lambda (s) (declare (ignore s))
              (terpri) (princ 4 (make-synonym-stream '*standard-output*))
              (fresh-line) (princ "b")))
  #.(%flp-nl "" "4" "b"))

;; A second FRESH-LINE after the first one wrote a newline adds nothing.
(deftest fresh-line-after-print.twice
  (%flp-run (lambda (s) (write-string "a" s) (fresh-line s) (fresh-line s)
              (princ "b" s)))
  #.(%flp-nl "a" "b"))

;; Printed text that itself ends in a newline leaves the stream at line start.
(deftest fresh-line-after-print.princ-ending-in-newline
  (%flp-run (lambda (s) (write-string "a" s) (princ #.(format nil "c~%") s)
              (fresh-line s) (princ "b" s)))
  #.(%flp-nl "ac" "b"))
