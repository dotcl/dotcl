;;; A quoted list of symbols in the compiler's own code (cross-compiled, so a recipe of symbol
;;; lookups and conses) is built once, not on every call: SPECIAL-OPERATOR-NAME-P is a MEMBER
;;; against such a list and used to cons 25 cells per call when the compiler was loaded from the SIL.

(deftest-compiled-only sil-recipe-constant-cached.no-consing-per-call
  (let ((f (find-symbol "SPECIAL-OPERATOR-NAME-P" "DOTCL.CIL-COMPILER"))
        (best nil))
    (dotimes (w 3)
      (let ((b0 (nth 4 (dotcl:gc-stats))))
        (dotimes (i 2000) (funcall f 'not-a-special-operator) (funcall f 'quote))
        (let ((d (- (nth 4 (dotcl:gc-stats)) b0)))
          (setf best (if best (min best d) d)))))
    (list (funcall f 'quote) (funcall f 'foo) (< best 40000)))
  (t nil t))

