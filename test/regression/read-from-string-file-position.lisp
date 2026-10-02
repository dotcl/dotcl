;;; A reader macro called by READ-FROM-STRING gets a string input stream, on
;;; which FILE-POSITION answers the index in the string (counting from the start
;;; of the string, not of :START), as on a stream from MAKE-STRING-INPUT-STREAM.
;;; It answered NIL, and Coalton's reader, which takes the source span of a
;;; form from it, failed with "Not a number: NIL" on (read-from-string "(...)").

(defun %rfsfp-read (string &rest args)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\! (lambda (s c) (declare (ignore c))
                               (list (file-position s) (typep s 'string-stream))))
    (multiple-value-list (apply #'read-from-string string args))))

(deftest read-from-string-file-position.in-reader-macro
  (list (%rfsfp-read "  !x")
        (%rfsfp-read "xx !" t nil :start 2))
  (((3 t) 3) ((4 t) 4)))

(deftest read-from-string-file-position.index-unchanged
  (list (multiple-value-list (read-from-string "(a b) c"))
        (multiple-value-list (read-from-string "ab cd" t nil :start 3))
        (multiple-value-list (read-from-string "ab cd" t nil :end 2))
        (multiple-value-list (read-from-string "x y" t nil :preserve-whitespace t)))
  (((a b) 6) (cd 5) (ab 2) (x 1)))
