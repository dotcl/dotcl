;;; TAILP takes a list: an atom other than NIL as the second argument is a type error,
;;; while a dotted list is walked to its atom tail.

(defmacro %tpl-error-p (form) `(handler-case (progn ,form nil) (type-error () t)))

(deftest tailp-list-argument.non-list-is-type-error
  (let ((n 5))
    (list (%tpl-error-p (tailp 5 n)) (%tpl-error-p (tailp nil n))
          (%tpl-error-p (tailp 'x n)) (%tpl-error-p (tailp "a" "a"))))
  (t t t t))

(deftest tailp-list-argument.proper-and-dotted
  (let* ((l (list 1 2 3 4)) (d (list* 'a 'b 'c)))
    (list (tailp (cddr l) l) (tailp l l) (tailp nil l) (tailp (list 3 4) l)
          (tailp nil nil) (tailp nil '(1 2))
          (tailp 'x d) (tailp 'c d) (tailp (cdr d) d)))
  (t t t nil t t nil t t))
