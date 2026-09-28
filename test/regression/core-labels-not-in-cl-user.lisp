;;; Starting from a SIL core (--asm compiler/cil-out.sil) must not leave the
;;; core's local and label names in CL-USER. The core writes them as plain
;;; symbols (BB_2974, ELSE_1183, |PKG653#:1_1|, ...); read in CL-USER they made
;;; it hold some 45000 symbols of its own, against a handful under SBCL, and
;;; anything that walks the package (DO-SYMBOLS, APROPOS, iterate's
;;; IN-PACKAGE generator test, which does a SET-DIFFERENCE of two such walks)
;;; paid for all of them. The FASL core never had them.
;;;
;;; The test files loaded before this one intern their own symbols in CL-USER,
;;; so the check counts only names shaped like a compiler temporary (a name
;;; ending in "_" and digits), with a bound far below the old count.

(defun %core-label-shaped-p (name)
  (let ((p (position #\_ name :from-end t)))
    (and p (< (1+ p) (length name))
         (every #'digit-char-p (subseq name (1+ p))))))

(deftest core-labels.not-interned-in-cl-user
  (let ((n 0) (pkg (find-package "COMMON-LISP-USER")))
    (do-symbols (s pkg)
      (when (and (eq (symbol-package s) pkg)
                 (%core-label-shaped-p (symbol-name s)))
        (incf n)))
    (< n 100))
  t)
