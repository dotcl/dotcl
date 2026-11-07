;;; With *READ-SUPPRESS* true an undefined sub-character of a dispatching macro
;;; character reads as nothing, as in SBCL: what follows it is read by whatever
;;; comes next. It used to take the next object, so `#f)` in a list that a
;;; #+ skipped ate the closing parenthesis (hu.dwim.quasi-quote's test file).

(defun %rsu-read (string)
  (handler-case (values (read-from-string string))
    (reader-error () :reader-error)
    (end-of-file () :eof)))

(deftest read-suppress-undefined-dispatch.skipped-list
  (list (%rsu-read "#+nil (def x (:a #f) (:b #t)) y")
        (%rsu-read "#+nil (a #q) y")
        (%rsu-read "#+nil (a #z b) y")
        (%rsu-read "#-(and) (a #f #t #q) y")
        (%rsu-read "#+nil (a #12q) y"))
  (y y y y y))

(deftest read-suppress-undefined-dispatch.nested
  (%rsu-read "(1 #+nil (2 (3 #f) #t) 4)")
  (1 4))

(deftest read-suppress-undefined-dispatch.invalid-stay-errors
  (list (%rsu-read "#+nil (a #<) y")
        (%rsu-read "#+nil (a #)) y"))
  (:reader-error :reader-error))

(deftest read-suppress-undefined-dispatch.not-suppressed
  (list (%rsu-read "(a #f)") (%rsu-read "#q"))
  (:reader-error :reader-error))
