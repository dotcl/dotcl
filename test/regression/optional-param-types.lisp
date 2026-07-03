;;; Regression tests: type declarations on &optional/&key parameters must be
;;; visible during the defun ANALYSIS pass. The macroexpansion cache shares one
;;; expansion between analysis and code-gen, and dotimes consults
;;; fixnum-typed-p on its count form at expansion time — if the analysis pass
;;; reported optional params as unbound, the cached expansion lacked the
;;; fixnum declaration and the whole native loop/aref path silently died for
;;; any (&optional (size N)) function (the cl-bench array benchmarks' shape).

(setf dotcl:*save-sil* t)

(defun %opt-fx-sum (&optional (n 10))
  (declare (fixnum n))
  (let ((s 0))
    (dotimes (i n)
      (setq s (+ s i)))
    s))

(defun %key-fx-sum (&key (n 10))
  (declare (fixnum n))
  (let ((s 0))
    (dotimes (i n)
      (setq s (+ s i)))
    s))

(deftest opt-param-fx-sum
  (list (%opt-fx-sum) (%opt-fx-sum 100))
  (45 4950))

(deftest key-param-fx-sum
  (%key-fx-sum :n 100)
  4950)

;; The dotimes counter of a fixnum-declared &optional count must take the
;; native path: no boxed Runtime.Increment in the emitted code.
(deftest opt-param-native-dotimes
  (let ((sil (princ-to-string (dotcl:function-sil #'%opt-fx-sum))))
    (notnot (search "Runtime.Increment" sil)))
  nil)

(deftest key-param-native-dotimes
  (let ((sil (princ-to-string (dotcl:function-sil #'%key-fx-sum))))
    (notnot (search "Runtime.Increment" sil)))
  nil)

(setf dotcl:*save-sil* nil)
