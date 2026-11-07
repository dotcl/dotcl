;;; SHADOWING-IMPORT inserts a different symbol as an internal symbol, whatever status the
;;; symbol it replaces had (CLHS). A symbol that is already present keeps its status.

(defpackage :si-status-a (:use))
(defpackage :si-status-b (:use))

(deftest shadowing-import-status.replaces-external-with-internal
  (let ((a (intern "X1" :si-status-a)) (b (intern "X1" :si-status-b)))
    (export a :si-status-a)
    (shadowing-import b :si-status-a)
    (multiple-value-bind (s st) (find-symbol "X1" :si-status-a)
      (list (eq s b) st (null (symbol-package a)) (and (member b (package-shadowing-symbols :si-status-a)) t))))
  (t :internal t t))

(deftest shadowing-import-status.present-symbol-keeps-status
  (let ((a (intern "X2" :si-status-a)))
    (export a :si-status-a)
    (shadowing-import a :si-status-a)
    (nth-value 1 (find-symbol "X2" :si-status-a)))
  :external)

(deftest shadowing-import-status.new-symbol-is-internal
  (let ((b (intern "X3" :si-status-b)))
    (shadowing-import b :si-status-a)
    (nth-value 1 (find-symbol "X3" :si-status-a)))
  :internal)
