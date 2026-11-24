;;; A comma outside a backquote is an error, except in text the reader is skipping.

(defun %cobs (string &key suppress)
  (handler-case (let ((*read-suppress* suppress)) (multiple-value-list (read-from-string string)))
    (reader-error () :reader-error)))

(deftest comma-outside-backquote-suppressed.unsuppressed
  (list (%cobs ",a") (%cobs ",@a") (%cobs ",.a"))
  (:reader-error :reader-error :reader-error))

(deftest comma-outside-backquote-suppressed.read-suppress
  (list (first (%cobs ",a" :suppress t))
        (first (%cobs ",@a b" :suppress t))
        (first (%cobs ",.a" :suppress t)))
  (nil nil nil))

(deftest comma-outside-backquote-suppressed.skipped-feature
  (list (read-from-string "(1 #+(or) ,@x 2)")
        (read-from-string "(1 #-common-lisp ,x 2)")
        (read-from-string "(1 #+(or) (a ,b ,@c) 2)"))
  ((1 2) (1 2) (1 2)))
