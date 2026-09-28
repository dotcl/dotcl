;;; Output order when user code writes to a pretty logical block.
;;;
;;; Inside a logical block FORMAT keeps what it prints in a buffer of its own
;;; and hands it to the pretty stream at ~_ and at the block's end. A ~/fn/
;;; function, a print-object method or a condition report reached from ~A / ~S
;;; runs its own FORMAT, and that one's ~_ went to the pretty stream at once:
;;; ahead of the text the caller had printed before it but still held. So
;;; "X~/fn/Y" with fn printing " ~:_Z" came out as " XZY". The expected strings
;;; are SBCL 2.6.8's.

(defun pusso-fill (s &rest r)
  (declare (ignore r))
  (format s " ~:_Z"))

(defun pusso-write-then-newline (s &rest r)
  (declare (ignore r))
  (write-string "abc" s)
  (format s " ~@:_Z"))

(defun pusso-cause (s c &rest r)
  (declare (ignore r))
  (format s "~@[ ~:_Caused by:~&~@<> ~@;~A~@:>~]" c))

(define-condition pusso-report (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "R ~:_Z"))))

(define-condition pusso-after (error) ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Mock Error." s))))

(defmethod print-object :after ((o pusso-after) s)
  (unless *print-escape*
    (format s " ~@:_See also:~&~<  ~@;~{~A~^~@:_~}~:>"
            (list '("FOO, bar" "FEZ")))))

(deftest pusso-slash-fill
  (format nil "~@<X~/pusso-fill/Y~:>" nil)
  "X ZY")

(deftest pusso-slash-written-then-newline
  (format nil "~@<| ~@;X~/pusso-write-then-newline/Y~:>" nil)
  "| Xabc
| ZY")

(deftest pusso-slash-nested-block
  (format nil "~@<Error occurred.~@:_~2@T~@<Foo.~/pusso-cause/~:>~@:>"
          "The number")
  "Error occurred.
  Foo. Caused by:
> The number")

(deftest pusso-report-fill
  (format nil "~@<X~AY~:>" (make-condition 'pusso-report))
  "XR ZY")

(deftest pusso-report-in-list
  (format nil "~@<a~{~A~^, ~}b~:>" (list (make-condition 'pusso-report) 1 2))
  "aR Z, 1, 2b")

(deftest pusso-print-object-after
  (format nil "~@<| ~@;~A~:>" (make-condition 'pusso-after))
  "| Mock Error.
| See also:
|   FOO, bar
|   FEZ")

(deftest pusso-outside-block-unchanged
  (princ-to-string (make-condition 'pusso-after))
  "Mock Error. See also:
  FOO, bar
  FEZ")
