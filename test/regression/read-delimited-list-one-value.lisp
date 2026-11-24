;;; READ-DELIMITED-LIST returns exactly one value, whatever the last reader macro returned.

(defun %rdl (string)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\% (lambda (s c) (declare (ignore s c)) (values)))
    (set-macro-character #\& (lambda (s c) (declare (ignore s c)) (values 1 2 3)))
    (with-input-from-string (i string)
      (multiple-value-list (read-delimited-list #\) i)))))

(deftest read-delimited-list-one-value.no-value-macro
  (list (%rdl "a %)") (%rdl "%)") (%rdl "(a) % 1)") (%rdl "#*1%)"))
  (((a)) (nil) (((a) 1)) ((#*1))))

(deftest read-delimited-list-one-value.many-value-macro
  (list (%rdl "a &)") (%rdl "&)"))
  (((a 1)) ((1))))
