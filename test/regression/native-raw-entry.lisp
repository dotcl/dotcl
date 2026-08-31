;;; The raw-argument and raw-return native entries, and what they do when the
;;; function they are called on has no such entry.
;;;
;;; A call site compiles against what a callee looked like at compile time, but
;;; the callee is fetched from its symbol on every call -- so the function that
;;; actually runs may have been redefined into one with no native entry, or one
;;; that does not return a fixnum at all. The entries below have to answer the
;;; same way the ordinary boxed call would, only slower, or (for a non-fixnum
;;; result) signal a TYPE-ERROR that names the value. Nothing emits calls to
;;; them yet; these tests are what keeps the fallback arms honest until the call
;;; sites arrive.

(defun nre-plain (x) (list :plain x))
(defun nre-fixnum (n) (declare (fixnum n)) (+ n 1))
(defun nre-two (a b) (declare (fixnum a b)) (+ a b))

;;; InvokeNative1 on a function with no native entry: boxes and goes through the
;;; ordinary path.
(deftest native-raw-entry.invoke-native-falls-back
  (dotcl::%invoke-native #'nre-plain 7)
  (:plain 7))

;;; ... and the answer matches the plain call.
(deftest native-raw-entry.invoke-native-agrees-with-funcall
  (equal (dotcl::%invoke-native #'nre-plain 7) (nre-plain 7))
  t)

;;; Raw return, one and two arguments. No raw entry is installed yet, so both go
;;; through the fallback and come back as ordinary integers.
(deftest native-raw-entry.raw-return-1
  (dotcl::%invoke-native-ret #'nre-fixnum 41)
  42)

(deftest native-raw-entry.raw-return-2
  (dotcl::%invoke-native-ret #'nre-two 20 22)
  42)

;;; A callee that does not return a fixnum must reach the caller as a TYPE-ERROR
;;; naming the value, not as a cast failure from inside the call sequence.
(deftest native-raw-entry.raw-return-non-fixnum-is-type-error
  (handler-case (progn (dotcl::%invoke-native-ret #'nre-plain 7) :no-error)
    (type-error (c) (list :type-error (and (search "fixnum" (string-downcase (format nil "~a" c))) t))))
  (:type-error t))

;;; The redefinition shape the fallback exists for: a function that had a native
;;; entry is replaced by one that cannot have one. The raw entry still answers,
;;; through the new definition.
(deftest native-raw-entry.redefined-callee-still-answers
  (progn
    (defun nre-redef (n) (declare (fixnum n)) (* n 2))
    (let ((before (dotcl::%invoke-native-ret #'nre-redef 21)))
      (defun nre-redef (n) (declare (fixnum n)) (+ n 100))
      (list before (dotcl::%invoke-native-ret #'nre-redef 21))))
  (42 121))
