;;; BUTLAST and NBUTLAST count conses, so N = 0 keeps a dotted tail.
;;;
;;; CLHS BUTLAST, NBUTLAST and DOTTED-LIST-ARGUMENTS:CLARIFY: the argument may
;;; be a dotted list, and with N zero no cons is omitted.

(deftest butlast-dotted.zero-keeps-tail
  (butlast '(1 2 . 3) 0)
  (1 2 . 3))

(deftest butlast-dotted.zero-copies-the-spine
  (let* ((l (cons 1 (cons 2 3)))
         (r (butlast l 0)))
    (list (eq r l) (equal r '(1 2 . 3)) (eq (cddr r) 3)))
  (nil t t))

(deftest butlast-dotted.counts-conses
  (list (butlast '(1 2 . 3) 1)
        (butlast '(1 2 . 3) 2)
        (butlast '(1 2 . 3) 3))
  ((1) nil nil))

(deftest nbutlast-dotted.zero-keeps-tail
  (let* ((l (cons 1 (cons 2 3)))
         (r (nbutlast l 0)))
    (list r l (eq r l)))
  ((1 2 . 3) (1 2 . 3) t))

(deftest nbutlast-dotted.counts-conses
  (let ((l1 (cons 1 (cons 2 3)))
        (l2 (cons 1 (cons 2 3))))
    (list (nbutlast l1 1) l1 (nbutlast l2 2) l2))
  ((1) (1) nil (1 2 . 3)))

(defun %bd-eval (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(deftest butlast-dotted.both-evaluators
  (list (%bd-eval :compile '(butlast '(1 2 . 3) 0))
        (%bd-eval :compile '(let ((l (cons 1 (cons 2 3))))
                              (list (nbutlast l 0) l)))
        (%bd-eval :interpret '(butlast '(1 2 . 3) 0))
        (%bd-eval :interpret '(let ((l (cons 1 (cons 2 3))))
                                (list (nbutlast l 0) l))))
  ((1 2 . 3) ((1 2 . 3) (1 2 . 3))
   (1 2 . 3) ((1 2 . 3) (1 2 . 3))))
