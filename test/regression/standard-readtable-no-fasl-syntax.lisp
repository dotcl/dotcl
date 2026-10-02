;;; The standard readtable carries only standard syntax, so a library is free
;;; to define #U or #K. The fasl loader reads its literals (#nU for an uninterned
;;; symbol, #K(...) for a structure) with a private readtable instead. When the
;;; standard readtable claimed #U, a library installing #u (puri does) looked to
;;; named-readtables like a conflict with :standard, and every readtable merged
;;; from :standard afterwards failed.

(deftest standard-readtable-no-sharp-u
  (get-dispatch-macro-character #\# #\U (copy-readtable nil))
  nil)

(deftest standard-readtable-no-sharp-k
  (get-dispatch-macro-character #\# #\K (copy-readtable nil))
  nil)

(deftest initial-readtable-sharp-u-is-user-definable
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\u
      (lambda (s c n) (declare (ignore c n)) (list :u (read s t nil t))))
    (values (read-from-string "#u\"x\"")
            (get-dispatch-macro-character #\# #\u (copy-readtable nil))))
  (:u "x") nil)

(deftest current-readtable-distinct-from-standard
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\u (lambda (s c n) (declare (ignore s c n)) 1))
    (not (eq (get-dispatch-macro-character #\# #\u)
             (get-dispatch-macro-character #\# #\u (copy-readtable nil)))))
  t)
