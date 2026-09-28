;;; Columns inside a FORMAT logical block when earlier text already reached the
;;; pretty stream.
;;;
;;; FORMAT keeps what it prints in a buffer of its own and hands it to the
;;; logical block at ~_ (and when user code reached from ~/fn/ or ~A runs a ~_
;;; of its own). Two things only counted that buffer:
;;; - a ~mincol<...~> segment is built as a string of its own (SBCL formats it to
;;;   a plain string stream), but a ~_ in user code inside it still wrote to the
;;;   block, so the text before it was missing from the segment and the padding
;;;   came out too wide;
;;; - ~T took the column from the buffer, so text already handed to the block
;;;   (by ~_, a nested directive, or user code) was not counted, nor was the
;;;   column the block started at.
;;; The expected strings are SBCL 2.6.8's.

(defun fuobc-fill (s &rest r)
  (declare (ignore r))
  (format s " ~:_Z"))

(defun fuobc-tab (s &rest r)
  (declare (ignore r))
  (format s "~10Tx"))

(defun fuobc-inner-block (s &rest r)
  (declare (ignore r))
  (format s "~@<in~8Tq~:>"))

(define-condition fuobc-report (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "R ~:_Z"))))

(deftest fuobc-segment-slash
  (format nil "~@<~12<x~/fuobc-fill/~;b~>~:>" 0)
  "x Z        b")

(deftest fuobc-segment-report
  (format nil "~@<~12<x~A~;b~>~:>" (make-condition 'fuobc-report))
  "xR Z       b")

(deftest fuobc-tab-after-report
  (format nil "~@<ab~Acd~20Te~:>" (make-condition 'fuobc-report))
  "abR Zcd             e")

(deftest fuobc-tab-after-slash
  (format nil "~@<ab~/fuobc-fill/cd~20Te~:>" 0)
  "ab Zcd              e")

(deftest fuobc-tab-after-fill
  (format nil "~@<ab~_cd~20Te~:>")
  "abcd                e")

(deftest fuobc-tab-block-not-at-column-0
  (format nil "xxxxx~@<ab~20Te~:>")
  "xxxxxab             e")

(deftest fuobc-tab-in-iteration
  (format nil "~@<ab~{~A~20T|~}~:>" '(1 2))
  "ab1                 |2 |")

(deftest fuobc-tab-in-user-code
  (format nil "~@<ab~/fuobc-tab/~:>" 0)
  "ab        x")

(deftest fuobc-tab-in-user-code-block
  (format nil "~@<ab~/fuobc-inner-block/~:>" 0)
  "abin    q")

(deftest fuobc-relative-tab-after-slash
  (format nil "~@<ab~/fuobc-fill/cd~5@Te~:>" 0)
  "ab Zcd     e")

(deftest fuobc-segment-then-tab
  (format nil "~@<~12<x~/fuobc-fill/~;b~>|~20T~A~:>" 0 (make-condition 'fuobc-report))
  "x Z        b|       R Z")

(deftest fuobc-segment-outside-block-unchanged
  (format nil "~12<x~/fuobc-fill/~;b~>" 0)
  "x Z        b")

(deftest fuobc-tab-outside-block-unchanged
  (format nil "ab~10Tc")
  "ab        c")

(deftest fuobc-tab-after-newline-unchanged
  (format nil "~@<> ~@;ab~&cd~10Te~:>")
  "> ab
> cd      e")
