;;; DEFSTRUCT :conc-name — an explicitly supplied prefix always builds a NEW
;;; accessor name and interns it in *PACKAGE*, even when the prefix is the empty
;;; string. Only a missing/NIL argument makes the accessor the slot symbol
;;; itself. dotcl conflated the two (both were treated as "accessor = slot
;;; symbol"), so (:conc-name "") on a slot named other-pkg::a defined the
;;; accessor in OTHER-PKG. Calls then only worked because the unqualified
;;; call-site bridge found the foreign symbol — the ansi-test structures-02
;;; cases (struct-test-36) leaned on exactly that.

(deftest defstruct-conc-name.empty-string-interns-in-current-package
  (progn
    (eval (read-from-string "(defpackage #:cn-slotpkg (:use))"))
    (eval (read-from-string "(defstruct (cn-s1 (:conc-name \"\")) cn-slotpkg::cn-a1)"))
    (let ((here (find-symbol "CN-A1" *package*))
          (there (find-symbol "CN-A1" "CN-SLOTPKG")))
      (list (and here (fboundp here) t)
            (and there (fboundp there) t)
            ;; the accessor reads the slot
            (funcall here (eval (read-from-string "(make-cn-s1 :cn-a1 7)"))))))
  (t nil 7))

(deftest defstruct-conc-name.nil-keeps-the-slot-symbol
  (progn
    (eval (read-from-string "(defstruct (cn-s2 (:conc-name nil)) cn-slotpkg::cn-a2)"))
    (let ((there (find-symbol "CN-A2" "CN-SLOTPKG")))
      (list (and there (fboundp there) t)
            ;; nothing interned in the reading package
            (find-symbol "CN-A2" *package*)
            (funcall there (eval (read-from-string "(make-cn-s2 :cn-a2 9)"))))))
  (t nil 9))

;;; A non-empty prefix keeps interning in the reading package (unchanged).
(deftest defstruct-conc-name.prefix-interns-in-current-package
  (progn
    (eval (read-from-string "(defstruct (cn-s3 (:conc-name \"CN3-\")) cn-slotpkg::cn-a3)"))
    (let ((here (find-symbol "CN3-CN-A3" *package*)))
      (list (and here (fboundp here) t)
            (funcall here (eval (read-from-string "(make-cn-s3 :cn-a3 5)"))))))
  (t 5))
