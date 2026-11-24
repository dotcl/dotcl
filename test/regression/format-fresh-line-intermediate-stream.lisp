;;; ~& written directly on the stream a ~/fn/ function or a PRINT-OBJECT method is handed asks about the
;;; line of the text that FORMAT has written before it (also inside ~( ~) and ~{ ~}), and FRESH-LINE on
;;; that stream says the same. The expected values are SBCL's.

(defun %ffi-fn (s a colon at &rest r) (declare (ignore a colon at r)) (format s "~&x") nil)
(defclass %ffi-po () ())
(defmethod print-object ((o %ffi-po) s) (format s "~&x"))

(defmacro %ffi-str (form) `(substitute #\| #\Newline ,form))

(deftest format-fresh-line-intermediate-stream.function-and-print-object
  (list (%ffi-str (format nil "x~/cl-user::%ffi-fn/" 1))
        (%ffi-str (format nil "x~{~/cl-user::%ffi-fn/~}" '(1)))
        (%ffi-str (format nil "x~a" (make-instance '%ffi-po)))
        (%ffi-str (format nil "x~s" (make-instance '%ffi-po)))
        (%ffi-str (format nil "x~(~a~)" (make-instance '%ffi-po)))
        (%ffi-str (format nil "x~(~/cl-user::%ffi-fn/~)" 1))
        (%ffi-str (format nil "x~{~a~}" (list (make-instance '%ffi-po))))
        (%ffi-str (format nil "~/cl-user::%ffi-fn/" 1))
        (%ffi-str (format nil "~a" (make-instance '%ffi-po)))
        (%ffi-str (format nil "x~%~a" (make-instance '%ffi-po)))
        (%ffi-str (with-output-to-string (s) (write-string "x" s) (format s "~a" (make-instance '%ffi-po))))
        (%ffi-str (with-output-to-string (s) (write-string "x" s) (princ (make-instance '%ffi-po) s))))
  ("x|x" "x|x" "x|x" "x|x" "x|x" "x|x" "x|x" "x" "x" "x|x" "x|x" "x|x"))
