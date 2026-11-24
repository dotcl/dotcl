;;; Prefix tests on text that comes from the program are ordinal. The default
;;; string comparison of .NET is culture-sensitive under ICU and skips ignorable
;;; characters (a soft hyphen, a zero-width space), so a source file that starts
;;; with a soft hyphen before the ";; -*- dotcl-compiled -*-" marker used to be
;;; taken for a compiled file under ICU and as plain source without it.

(defvar *opt-shy* nil)

(deftest ordinal-prefix-tests.soft-hyphen-before-compiled-marker-is-source
  (let ((file (format nil "~A/ordinal-prefix-~A.lisp" (regression-temp-dir) (get-internal-real-time))))
    (with-open-file (o file :direction :output :if-exists :supersede)
      (write-char (code-char #xAD) o)
      (format o ";; -*- dotcl-compiled -*-~%(setq *opt-shy* 42)~%"))
    (setq *opt-shy* nil)
    ;; As source the soft hyphen is a one-character symbol; give it a value.
    (setf (symbol-value (intern (string (code-char #xAD)))) 1)
    (ignore-errors (load file))
    (ignore-errors (delete-file file))
    *opt-shy*)
  42)
