;;; PRINC / PRIN1 / WRITE to a string output stream whose current line is long
;;; take time independent of that length. The column the object starts at was
;;; found before every print by scanning the stream's text back to its last
;;; newline, so building one long line piece by piece took quadratic time.

(defun %pll-seconds (fn)
  (let ((t0 (get-internal-real-time)))
    (funcall fn)
    (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))

(defun %pll-build (print-fn n)
  (with-output-to-string (s)
    (loop repeat n do (funcall print-fn s))))

(deftest print-long-line-string-stream.length
  (list (length (%pll-build (lambda (s) (princ "test" s)) 60000))
        (length (%pll-build (lambda (s) (prin1 12 s)) 60000))
        (length (%pll-build (lambda (s) (write "ab" :stream s :escape nil)) 60000)))
  (240000 120000 120000))

(deftest print-long-line-string-stream.time
  (let ((secs (%pll-seconds
               (lambda ()
                 (%pll-build (lambda (s) (princ "test" s)) 60000)
                 (%pll-build (lambda (s) (prin1 12 s)) 60000)))))
    (if (< secs 15) :ok secs))
  :ok)

;;; The same for FORMAT: the stream's column is needed only by ~T, a logical
;;; block, or an object whose PRINT-OBJECT method is told where it starts.
(deftest print-long-line-string-stream.format
  (let ((secs (%pll-seconds
               (lambda ()
                 (%pll-build (lambda (s) (format s "~a" "test")) 60000)
                 (%pll-build (lambda (s) (format s "~d-~s" 1 "x")) 60000)))))
    (if (< secs 15) :ok secs))
  :ok)

(deftest print-long-line-string-stream.format-column
  (with-output-to-string (s)
    (write-string "abc" s)
    (format s "~a~8Tx" "de"))
  "abcde   x")
