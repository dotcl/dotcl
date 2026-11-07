;;; DO-SYMBOLS / DO-EXTERNAL-SYMBOLS see every table change made between two walks of the same
;;; package (the walk reuses a per-package array of symbols until a table changes).

(defun %dsc-names (p ext)
  (let (r)
    (if ext
        (do-external-symbols (s p) (push (symbol-name s) r))
        (do-symbols (s p) (when (eq (symbol-package s) p) (push (symbol-name s) r))))
    (sort r #'string<)))

(defun %dsc-pkg (name)
  (when (find-package name) (delete-package name))
  (make-package name :use nil))

(deftest do-symbols-cache.intern-export
  (let ((p (%dsc-pkg "DSC1")))
    (list (%dsc-names p nil)
          (progn (intern "A" p) (%dsc-names p nil))
          (progn (export (intern "B" p) p) (list (%dsc-names p nil) (%dsc-names p t)))
          (progn (unexport (find-symbol "B" p) p) (list (%dsc-names p nil) (%dsc-names p t)))
          (progn (unintern (find-symbol "A" p) p) (%dsc-names p nil))))
  (NIL ("A") (("A" "B") ("B")) (("A" "B") NIL) ("B")))

(deftest do-symbols-cache.shadow-import-use
  (let ((p (%dsc-pkg "DSC2")) (q (%dsc-pkg "DSC2Q")))
    (export (intern "X" q) q)
    (list (%dsc-names p nil)
          (let (r) (do-symbols (s p) (push (symbol-name s) r)) r)
          (progn (use-package q p) (let (r) (do-symbols (s p) (push (symbol-name s) r)) r))
          (progn (shadow "X" p) (let (r) (do-symbols (s p) (push (symbol-package s) r)) (list (length r) (eq (car r) p))))
          (progn (shadowing-import (intern "Y" q) p) (%dsc-names p nil))
          (progn (import (intern "Z" q) p) (%dsc-names p nil))))
  (NIL NIL ("X") (1 T) ("X") ("X")))
