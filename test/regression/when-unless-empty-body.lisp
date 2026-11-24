;;; WHEN / UNLESS with no body expand to a two-argument IF whose then-form is NIL (not an empty
;;; PROGN, which a code walker may drop, leaving a one-argument IF).

(deftest when-unless-empty-body.expansion
  (list (macroexpand-1 '(when t))
        (macroexpand-1 '(unless x))
        (macroexpand-1 '(when x 1 2))
        (macroexpand-1 '(unless x 1)))
  ((if t nil) (if (not x) nil) (if x (progn 1 2)) (if (not x) (progn 1))))

(deftest when-unless-empty-body.value
  (list (when t) (unless nil) (when nil 1) (unless t 1) (when t 1 2))
  (nil nil nil nil 2))
