;;; A .NET method that returns void produces no values.
;;;
;;; Reflection reports a void return and a null return identically -- both arrive
;;; as a null object -- so the question cannot be answered from the result. There
;;; are two places that answer it, and they have to agree:
;;;
;;;   compiled typed call   the overload is already resolved, so its MethodInfo
;;;                         says exactly whether it is void
;;;   reflective call       dotnet:invoke / dotnet:static reach the method either
;;;                         through a cached MethodInfo or through
;;;                         Type.InvokeMember, whose binder resolves the overload
;;;                         internally and never says which one it picked. The
;;;                         answer is therefore keyed on (type, name, argument
;;;                         count), which is the same whichever path runs.
;;;
;;; Keying the reflective side on argument TYPES would make the same call answer
;;; differently depending on the path, since InvokeMember is used for things as
;;; ordinary as passing NIL.

;;; --- void produces no values ----------------------------------------------

;;; Written as a length: the test framework reads a bare () as "returns no
;;; values", which is the assertion being made, but stating it as a count keeps
;;; the two readings from being confused.
(deftest dotnet-void-returns-no-values
  (length (multiple-value-list
           (dotnet:invoke (dotnet:new "System.Collections.ArrayList") "Clear")))
  0)

;;; The same call reached without the compiler's typed lowering.
(deftest dotnet-void-returns-no-values-reflective
  (let ((al (dotnet:new "System.Collections.ArrayList")))
    (length (multiple-value-list (funcall #'dotnet:invoke al "Clear"))))
  0)

;;; A zero-valued form used where one value is wanted is NIL, as for any CL form
;;; that returns nothing. This is what keeps the change from breaking callers.
(deftest dotnet-void-in-value-position-is-nil
  (list (dotnet:invoke (dotnet:new "System.Collections.ArrayList") "Clear"))
  (nil))

;;; --- a method that returns null still returns one value -------------------

;;; Indistinguishable from the void case by result; not by signature.
(deftest dotnet-null-result-is-one-value
  (multiple-value-list
   (dotnet:invoke (dotnet:new "System.Collections.Hashtable") "get_Item" "absent"))
  (nil))

(deftest dotnet-non-null-result-unchanged
  (multiple-value-list (dotnet:invoke "abc" "ToString"))
  ("abc"))

(deftest dotnet-value-returning-call-unchanged
  (multiple-value-list
   (dotnet:invoke (dotnet:new "System.Collections.ArrayList") "get_Count"))
  (0))

;;; --- the value count must not leak out of a call ---------------------------

;;; DOTNET:DEFINE-CLASS lives in contrib. Load it here rather than rely on an
;;; earlier file having required it: that file does not run in every mode.
(require :dotnet-class)

;;; A constructor body is Lisp and may end in a void setter. The instance is one
;;; value regardless, and so is anything computed from it: with the zero left
;;; standing, (dotnet:invoke (dotnet:new C) "get_N") returned no values at all.
(deftest dotnet-void-does-not-leak-through-constructor
  (progn
    (dotnet:define-class "DotclTest.VoidValuesA" (Object)
      (:properties ("N" Int32))
      (:ctor () (dotnet:invoke self "set_N" 7)))
    (multiple-value-list
     (dotnet:invoke (dotnet:new "DotclTest.VoidValuesA") "get_N")))
  (7))

;;; The same with the instance bound first -- this path always worked, and is
;;; here so a regression in one is distinguishable from the other.
(deftest dotnet-constructor-value-when-bound-first
  (progn
    (dotnet:define-class "DotclTest.VoidValuesB" (Object)
      (:properties ("N" Int32))
      (:ctor () (dotnet:invoke self "set_N" 9)))
    (let ((o (dotnet:new "DotclTest.VoidValuesB")))
      (multiple-value-list (dotnet:invoke o "get_N"))))
  (9))

;;; DOTNET:NEW is one value even when the constructor ends in a void call.
(deftest dotnet-new-is-one-value
  (progn
    (dotnet:define-class "DotclTest.VoidValuesC" (Object)
      (:properties ("N" Int32))
      (:ctor () (dotnet:invoke self "set_N" 1)))
    (length (multiple-value-list (dotnet:new "DotclTest.VoidValuesC"))))
  1)

;;; A void call followed by a value-returning one leaves the second one's count.
(deftest dotnet-void-then-value
  (let ((al (dotnet:new "System.Collections.ArrayList")))
    (multiple-value-list
     (progn (dotnet:invoke al "Clear")
            (dotnet:invoke al "get_Count"))))
  (0))
