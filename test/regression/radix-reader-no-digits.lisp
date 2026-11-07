;;; #B #O #X #R need at least one digit: a bare sign or an empty token is a
;;; reader error, not zero.

(defun %rr-read (s)
  (handler-case (read-from-string s)
    (reader-error () :reader-error)
    (end-of-file () :eof)
    (error () :error)))

(deftest radix-reader.sign-only-is-error
  (mapcar (lambda (s) (eq (%rr-read s) 0))
          '("#b-" "#x+" "#o- " "#3r-" "#36r+"))
  (nil nil nil nil nil))

(deftest radix-reader.empty-token-is-error
  (mapcar (lambda (s) (eq (%rr-read s) 0))
          '("#x " "#b)" "#x||"))
  (nil nil nil))

(deftest radix-reader.valid-forms-still-read
  (list (%rr-read "#x-FF") (%rr-read "#b+101") (%rr-read "#3r12") (%rr-read "#x1/F"))
  (-255 5 5 1/15))
