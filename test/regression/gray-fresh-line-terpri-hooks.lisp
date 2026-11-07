;;; FORMAT ~& at the start of a control string calls the user's STREAM-FRESH-LINE; PRINT
;;; and PPRINT start their line with STREAM-TERPRI (not a STREAM-WRITE-CHAR of a newline).
;;; STREAM-START-LINE-P is no longer asked for a leading ~&.
(require "dotcl-gray")

(defclass %gflt (dotcl-gray:fundamental-character-output-stream)
  ((calls :initform nil :accessor %gflt-calls) (col :initform 0 :accessor %gflt-col)))
(defmethod dotcl-gray:stream-write-char ((s %gflt) c)
  (setf (%gflt-col s) (if (char= c #\Newline) 0 (1+ (%gflt-col s)))) c)
(defmethod dotcl-gray:stream-line-column ((s %gflt)) (%gflt-col s))
(defmethod dotcl-gray:stream-start-line-p ((s %gflt)) (push :start-line-p (%gflt-calls s)) (zerop (%gflt-col s)))
(defmethod dotcl-gray:stream-fresh-line ((s %gflt))
  (push :fresh-line (%gflt-calls s))
  (if (zerop (%gflt-col s)) nil (progn (dotcl-gray:stream-write-char s #\Newline) t)))
(defmethod dotcl-gray:stream-terpri ((s %gflt))
  (push :terpri (%gflt-calls s)) (dotcl-gray:stream-write-char s #\Newline) nil)

(defun %gflt-calls-of (fn)
  (let ((s (make-instance '%gflt))) (funcall fn s) (reverse (%gflt-calls s))))

(deftest gray-fresh-line-terpri-hooks.format-fresh-line
  (list (%gflt-calls-of (lambda (s) (write-string "ab" s) (format s "~&x")))
        (%gflt-calls-of (lambda (s) (format s "~&x"))))
  ((:fresh-line) (:fresh-line)))

(deftest gray-fresh-line-terpri-hooks.print-and-pprint
  (list (%gflt-calls-of (lambda (s) (print 1 s)))
        (%gflt-calls-of (lambda (s) (pprint 1 s)))
        (%gflt-calls-of (lambda (s) (terpri s))))
  ((:terpri) (:terpri) (:terpri)))

(deftest gray-fresh-line-terpri-hooks.output-unchanged
  (let ((s (make-instance 'dotcl-gray:fundamental-character-output-stream)))
    (declare (ignorable s))
    (list (with-output-to-string (o) (write-string "ab" o) (format o "~&x") (print 1 o))
          (with-output-to-string (o) (format o "~&x"))))
  ("ab
x
1 " "x"))
