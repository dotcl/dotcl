;;; RECURSIVE-P of READ-LINE and READ-CHAR-NO-HANG does not override EOF-ERROR-P.

(deftest read-line-recursive-p.eof-value
  (list (multiple-value-list (read-line (make-string-input-stream "") nil 'x t))
        (multiple-value-list (read-line (make-string-input-stream "") nil 'x nil))
        (read-char-no-hang (make-string-input-stream "") nil 'x t))
  ((x t) (x t) x))

(deftest read-line-recursive-p.error-still-signalled
  (list (handler-case (read-line (make-string-input-stream "") t 'x t)
          (end-of-file () :eof))
        (handler-case (read-char-no-hang (make-string-input-stream "") t 'x t)
          (end-of-file () :eof)))
  (:eof :eof))
