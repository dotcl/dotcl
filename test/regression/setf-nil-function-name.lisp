;;; (SETF NIL) and (SETF T) are function names like any (SETF symbol): NIL and T
;;; are symbols. FBOUNDP, FDEFINITION and FMAKUNBOUND checked for the symbol
;;; class, which NIL and T are not represented by, and signalled TYPE-ERROR.
;;; Found by ansi-test's random type propagation tests (RANDOM-TYPE-PROP.FBOUNDP.2).

(deftest setf-nil-function-name
  (list (fboundp '(setf nil))
        (fboundp '(setf t))
        (equal (fmakunbound '(setf nil)) '(setf nil))
        (handler-case (fdefinition '(setf t)) (undefined-function () :undefined))
        (handler-case (fboundp '(setf 3)) (type-error () :type-error)))
  (nil nil t :undefined :type-error))
