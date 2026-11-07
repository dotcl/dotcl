;;; A symbol whose name is decimal number syntax (1.5, .5, 1e5, 1.) reads as a number in
;;; every *read-base*, so the printer escapes it whatever *print-base* is.

(deftest print-symbol-decimal-in-base.escaped
  (let ((out '()))
    (dolist (base '(2 8 10 16))
      (let ((*print-base* base))
        (dolist (name '("1.5" "-1.5" ".5" "1." "1.5e3" "1e5" "1.5d0"))
          (let* ((s (intern name :cl-user))
                 (text (prin1-to-string s))
                 (back (let ((*read-base* base)) (read-from-string text))))
            (unless (eq s back) (push (list base name text) out))))))
    out)
  nil)
