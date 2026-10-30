;;; A skipped form before the delimiter is treated like whitespace.

(defun %rdl-skip-read (string)
  (with-input-from-string (s string)
    (read-delimited-list #\) s)))

(deftest read-delimited-list-skipped.feature-at-end
  (%rdl-skip-read "a #+nil b)")
  (a))

(deftest read-delimited-list-skipped.feature-list-at-end
  (%rdl-skip-read "a #+nil (b c))")
  (a))

(deftest read-delimited-list-skipped.negative-feature-includes
  (%rdl-skip-read "a #-nil b)")
  (a b))

(deftest read-delimited-list-skipped.present-feature-includes
  (%rdl-skip-read "a #+dotcl b)")
  (a b))

(deftest read-delimited-list-skipped.block-comment-at-end
  (%rdl-skip-read "a #|x|#)")
  (a))

(deftest read-delimited-list-skipped.line-comment-at-end
  (%rdl-skip-read "a ;x
)")
  (a))

(deftest read-delimited-list-skipped.read-time-eval-with-no-values
  (%rdl-skip-read "a #.(values))")
  (a))

(deftest read-delimited-list-skipped.comment-between-elements
  (%rdl-skip-read "a #|x|# b)")
  (a b))

(deftest read-delimited-list-skipped.ordinary-list
  (values (read-from-string "(a #+nil (b c) #|x|# ;y
 d)"))
  (a d))

(defun %rdl-brace-readtable ()
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\{ (lambda (s c)
                               (declare (ignore c))
                               (cons :braces (read-delimited-list #\} s t)))
                         nil rt)
    (set-macro-character #\} (get-macro-character #\)) nil rt)
    rt))

(defun %rdl-brace-read (string)
  (let ((*readtable* (%rdl-brace-readtable)))
    (handler-case (values (read-from-string string))
      (error (e) (list :error (type-of e))))))

(deftest read-delimited-list-skipped.brace-feature-at-end
  (%rdl-brace-read "{a #+nil b}")
  (:braces a))

(deftest read-delimited-list-skipped.brace-feature-list-at-end
  (%rdl-brace-read "{a #+nil (b c)}")
  (:braces a))

(deftest read-delimited-list-skipped.brace-block-comment-at-end
  (%rdl-brace-read "{a #|x|#}")
  (:braces a))

(deftest read-delimited-list-skipped.brace-read-time-eval-with-no-values
  (%rdl-brace-read "{a #.(values)}")
  (:braces a))

(deftest read-delimited-list-skipped.brace-nested
  (%rdl-brace-read "{a {b #+nil c}}")
  (:braces a (:braces b)))
