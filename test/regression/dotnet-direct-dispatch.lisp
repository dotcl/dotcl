;;; Typed direct-callvirt codegen: direct dispatch for a known-typed dotnet:invoke.
;;; %dotnet-call-direct emits a direct callvirt to the resolved .NET method (no
;;; runtime InvokeMember / member lookup). Shape:
;;;   (%dotnet-call-direct "Type.FullName" "Method" (param-type-strings...) recv arg...)
;;; This proves the emitter path; the type-declared surface / compiler-macro and
;;; type inference are built on top of it.

;; Zero-arg, reference-type result (StringBuilder.ToString -> System.String).
(deftest direct-dispatch-tostring
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "hello")
    (%dotnet-call-direct "System.Text.StringBuilder" "ToString" () sb))
  "hello")

;; Zero-arg, value-type result (StringBuilder.get_Length -> int -> Fixnum).
(deftest direct-dispatch-length
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "abcd")
    (%dotnet-call-direct "System.Text.StringBuilder" "get_Length" () sb))
  4)

;; The direct call produces the same result as the InvokeMember path.
(deftest direct-dispatch-matches-invoke
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "xyz")
    (string= (%dotnet-call-direct "System.Text.StringBuilder" "ToString" () sb)
             (dotnet:invoke sb "ToString")))
  t)

;; Reference-type argument marshaling: Append(string) chosen by the param type,
;; arg marshaled LispString -> System.String.
(deftest direct-dispatch-append-string
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.String") sb "AB")
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.String") sb "CD")
    (dotnet:invoke sb "ToString"))
  "ABCD")

;; Value-type argument marshaling: Append(int) chosen over other overloads, arg
;; marshaled Fixnum -> System.Int32 (unbox).
(deftest direct-dispatch-append-int
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.Int32") sb 42)
    (dotnet:invoke sb "ToString"))
  "42")

;; Overload selection by param type: same name+arg-count, different param type, same
;; receiver -> the declared overload is the one that runs.
(deftest direct-dispatch-overload-by-paramtype
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.String") sb "7")
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.Int32") sb 7)
    (dotnet:invoke sb "ToString"))
  "77")

;;; --- typed surface: (dotnet:invoke (the (dotnet "T") r) "M" ...) lowering ---
;; A type-declared receiver lowers to %dotnet-call-direct via the compiler macro.
(deftest direct-surface-length
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                   (the (dotnet "System.String") "abcde"))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "get_Length"))
  5)

(deftest direct-surface-tostring
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                   (the (dotnet "System.String") "hi"))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "ToString"))
  "hi")

;; Untyped receiver: the compiler macro declines and the dynamic path is used.
(deftest direct-surface-untyped-declines
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "world")
    (dotnet:invoke sb "ToString"))
  "world")

;; Per-arg-local codegen: a multi-arg typed invoke marshals each argument
;; from its own local (no LispObject[] array). String.IndexOf(string, int).
(deftest direct-surface-multi-arg
  (let ((s (dotnet:box "hello, hello" "System.String")))
    (dotnet:invoke (the (dotnet "System.String") s) "IndexOf"
                   (the (dotnet "System.String") "hello")
                   (the (dotnet "System.Int32") 3)))
  7)

;; Two-arg typed call whose result feeds another: confirms locals don't clash
;; across nested direct calls.
(deftest direct-surface-multi-arg-nested
  (let ((s (dotnet:box "abcabc" "System.String")))
    (dotnet:invoke (the (dotnet "System.String") s) "Substring"
                   (the (dotnet "System.Int32") 1)
                   (the (dotnet "System.Int32") 3)))
  "bca")

;;; DOTNET:BOX and DOTNET:NEW are static type sources too, so a
;;; typed direct call needs no explicit THE when the receiver/args are already
;;; written as box/new forms (their literal type is known at compile time).
(deftest direct-surface-box-receiver-and-arg
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (dotnet:box sb "System.Text.StringBuilder") "Append"
                   (dotnet:box "abcde" "System.String"))
    (dotnet:invoke (dotnet:box sb "System.Text.StringBuilder") "get_Length"))
  5)

(deftest direct-surface-new-receiver
  ;; A (dotnet:new "T" ...) receiver lowers directly (here the 0-arg get_Length).
  (dotnet:invoke (dotnet:new "System.Text.StringBuilder"
                             (dotnet:box "abcd" "System.String"))
                 "get_Length")
  4)

(deftest direct-surface-box-mixed-with-the
  ;; Receiver via THE, argument via BOX: both recognized.
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                   (dotnet:box "xy" "System.String"))
    (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "ToString"))
  "xy")

;; Partial typing (typed receiver, untyped arg) declines to the dynamic path
;; and still returns the right value.
(deftest direct-surface-box-partial-declines
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (dotnet:box sb "System.Text.StringBuilder") "Append" "zz")
    (dotnet:invoke sb "ToString"))
  "zz")

;;; Value-type receivers: direct dispatch unboxes the receiver and uses
;;; `call` for non-virtual methods / `constrained. callvirt` for virtual ones.
(deftest direct-value-receiver-virtual-tostring
  ;; Int32.ToString() is virtual -> constrained.callvirt.
  (dotnet:invoke (the (dotnet "System.Int32") (dotnet:box 42 "System.Int32")) "ToString")
  "42")

(deftest direct-value-receiver-nonvirtual-getter
  ;; DateTime.get_Year is non-virtual -> plain call on the unboxed pointer.
  (dotnet:invoke (the (dotnet "System.DateTime")
                      (dotnet:new "System.DateTime"
                                  (the (dotnet "System.Int32") 2020)
                                  (the (dotnet "System.Int32") 1)
                                  (the (dotnet "System.Int32") 15)))
                 "get_Year")
  2020)

(deftest direct-value-receiver-with-value-arg
  ;; Int32.CompareTo(Int32): value receiver + value-type argument. 5 < 8 -> -1.
  (dotnet:invoke (the (dotnet "System.Int32") (dotnet:box 5 "System.Int32"))
                 "CompareTo" (the (dotnet "System.Int32") 8))
  -1)

(deftest direct-value-receiver-enum-tostring
  (dotnet:invoke (dotnet:box "Read, Write" "System.IO.FileShare") "ToString")
  "ReadWrite")

;; /: the slot-share / single-ref passes must enumerate locals carried in
;; :dotnet-call-direct-locals operand lists (RECV + ARGs), not just top-level
;; :ldloc/:stloc. Here SB lives across three typed Append calls while the extra
;; LispObject let* locals (A B C) give the slot-merge pass disjoint candidates to
;; coalesce. If the nested DRCV/DARG references were invisible to the merge scan,
;; a coalesced slot would clobber a still-live arg (assembly error / wrong
;; output). Centralized enumeration keeps them live; result must be "x22z".
(deftest direct-locals-slot-share-keeps-nested-args
  (let* ((sb (dotnet:new "System.Text.StringBuilder"))
         (a (progn (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.String") sb "x") 1))
         (b (progn (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.Int32") sb 22) 2))
         (c (progn (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.String") sb "z") 3)))
    (declare (ignore a b c))
    (%dotnet-call-direct "System.Text.StringBuilder" "ToString" () sb))
  "x22z")

;; A typed call names a property without get_, and on an interface type reaches
;; the members of the interfaces it extends (IList.Count is ICollection's). Both
;; used to fall back to the reflective path at every call.
(defun %ddd-count (l) (dotnet:invoke (the (dotnet "System.Collections.IList") l) "Count"))
(defun %ddd-item (l) (dotnet:invoke (the (dotnet "System.Collections.IList") l) "Item"
                                    (the (dotnet "System.Int32") 1)))
(defun %ddd-length (sb) (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Length"))

(deftest direct-property-and-inherited-interface-member
  (let ((l (dotnet:new "System.Collections.ArrayList"))
        (sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke l "Add" "a") (dotnet:invoke l "Add" "b")
    (dotnet:invoke sb "Append" "abc")
    (list (%ddd-count l) (%ddd-item l) (%ddd-length sb)))
  (2 "b" 3))

(deftest-compiled-only direct-property-and-inherited-interface-member-no-reflection
  (let ((l (dotnet:new "System.Collections.ArrayList")))
    (dotnet:invoke l "Add" "a")
    (%ddd-count l)
    (let ((b0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
      (dotimes (i 1000) (%ddd-count l))
      ;; The reflective path allocates over 250 bytes a call.
      (< (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") b0) 100000)))
  t)

;;; --- typed dotnet:static: (dotnet:static "T" "M" (the (dotnet "P") a) ...) ---
;; Literal type and member, every argument typed (or none): a direct call, getter
;; or field load. Anything else, or a member that does not resolve, stays on the
;; reflective path with the same result.
(defun %dsd-max (a b) (dotnet:static "System.Math" "Max" (the (dotnet "System.Int32") a)
                                     (the (dotnet "System.Int32") b)))
(defun %dsd-max-untyped (a b) (dotnet:static "System.Math" "Max" a b))
(defun %dsd-concat (a b) (dotnet:static "System.String" "Concat" (the (dotnet "System.String") a)
                                        (the (dotnet "System.String") b)))
(defun %dsd-abs-int (a) (dotnet:static "System.Math" "Abs" (the (dotnet "System.Int32") a)))
(defun %dsd-abs-double (a) (dotnet:static "System.Math" "Abs" (the (dotnet "System.Double") a)))
(defun %dsd-concat-objects (a b) (dotnet:static "System.String" "Concat" (the (dotnet "System.Object") a)
                                                (the (dotnet "System.Object") b)))
(defun %dsd-utf8 () (dotnet:static "System.Text.Encoding" "UTF8"))
(defun %dsd-utf8-count (s) (dotnet:invoke (dotnet:static "System.Text.Encoding" "UTF8") "GetByteCount"
                                          (the (dotnet "System.String") s)))
(defun %dsd-parse (s) (dotnet:static "System.Int32" "Parse" (the (dotnet "System.String") s)))
(defun %dsd-no-member (a) (dotnet:static "System.Math" "NoSuchMember" (the (dotnet "System.Int32") a)))
(defun %dsd-no-type () (dotnet:static "No.Such.Type" "Value"))
(defun %dsd-keep-alive (x) (dotnet:static "System.GC" "KeepAlive" (the (dotnet "System.Object") x)))

(deftest static-direct-typed-methods
  (list (%dsd-max 3 9) (%dsd-concat "ab" "cd") (%dsd-max-untyped 3 9))
  (9 "abcd" 9))

(deftest static-direct-overload-by-paramtype
  (list (%dsd-abs-int -3) (%dsd-abs-double -2.5d0) (%dsd-concat-objects 1 "x"))
  (3 2.5d0 "1x"))

(deftest static-direct-properties-and-fields
  (list (dotnet:invoke (%dsd-utf8) "WebName")
        (%dsd-utf8-count "abc")
        (dotnet:static "System.String" "Empty")
        (dotnet:static "System.Int32" "MaxValue")
        (dotnet:static "System.Math" "PI")
        (equal (princ-to-string (dotnet:static "System.DayOfWeek" "Monday"))
               (princ-to-string (funcall #'dotnet:static "System.DayOfWeek" "Monday")))
        (= (dotnet:invoke (dotnet:static "System.DateTime" "Now") "Year")
           (dotnet:invoke (funcall #'dotnet:static "System.DateTime" "Now") "Year")))
  ("utf-8" 3 "" 2147483647 3.141592653589793d0 t t))

(deftest static-direct-unresolved-falls-back
  (list (handler-case (%dsd-no-member 1) (error () :error))
        (handler-case (%dsd-no-type) (error () :error))
        (multiple-value-list (%dsd-keep-alive 1)))
  (:error :error nil))

;; A .NET exception from the direct call is an ERROR carrying the .NET type, as
;; it is on the reflective path.
(deftest static-direct-exception
  (list (handler-case (%dsd-parse "nope")
          (error (e) (dotnet:exception-typep e "System.FormatException")))
        (handler-case (funcall #'dotnet:static "System.Int32" "Parse" "nope")
          (error (e) (dotnet:exception-typep e "System.FormatException")))
        (%dsd-parse "42"))
  (t t 42))

(deftest-compiled-only static-direct-no-reflection
  (progn
    (%dsd-max 1 2) (%dsd-utf8)
    (let ((b0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
      (dotimes (i 1000) (%dsd-max 1 2) (%dsd-utf8))
      ;; The reflective path allocates over 1700 bytes for the pair.
      (< (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") b0) 300000)))
  t)

;;; A typed receiver or argument goes through the same marshalling as the
;;; untyped path: a Lisp string where System.String is declared, a fixnum
;;; where Int32 / Int64 / Double / Char / Object / IComparable is.
(defun %typed-id (x) x)

(defmacro %same-typed-untyped (typed untyped)
  `(equal (multiple-value-list ,typed) (multiple-value-list ,untyped)))

(deftest direct-receiver-lisp-string
  (let ((s (%typed-id "hello")))
    (list
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.String") s) "Length")
                          (dotnet:invoke s "Length"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.String") s) "ToUpper")
                          (dotnet:invoke s "ToUpper"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.String") s) "Substring"
                                         (the (dotnet "System.Int32") (%typed-id 2)))
                          (dotnet:invoke s "Substring" 2))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.String") s) "Replace"
                                         (the (dotnet "System.Char") (%typed-id #\l))
                                         (the (dotnet "System.Char") (%typed-id #\L)))
                          (dotnet:invoke s "Replace" #\l #\L))
     (dotnet:invoke (the (dotnet "System.String") s) "Length")
     (dotnet:invoke (the (dotnet "System.Object") s) "ToString")
     (dotnet:invoke (the (dotnet "System.IComparable") s) "CompareTo"
                    (the (dotnet "System.Object") (%typed-id "a")))))
  (t t t t 5 "hello" 1))

(deftest direct-receiver-numbers-and-char
  (let ((i (%typed-id 42)) (l (%typed-id 5000000000)) (d (%typed-id 1.5d0))
        (f (%typed-id 1.5f0)) (c (%typed-id #\a)))
    (list
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Int32") i) "ToString")
                          (dotnet:invoke i "ToString"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Int32") i) "CompareTo"
                                         (the (dotnet "System.Int32") (%typed-id 7)))
                          (dotnet:invoke i "CompareTo" 7))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Int64") l) "ToString")
                          (dotnet:invoke l "ToString"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Double") d) "ToString")
                          (dotnet:invoke d "ToString"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Single") f) "ToString")
                          (dotnet:invoke f "ToString"))
     (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Char") c) "ToString")
                          (dotnet:invoke c "ToString"))
     (dotnet:invoke (the (dotnet "System.Object") i) "ToString")))
  (t t t t t t "42"))

(deftest direct-receiver-enum-wrapped
  (list
   (dotnet:invoke (the (dotnet "System.DayOfWeek")
                       (%typed-id (dotnet:static "System.DayOfWeek" "Monday")))
                  "ToString")
   (dotnet:invoke (the (dotnet "System.Text.StringBuilder")
                       (%typed-id (dotnet:new "System.Text.StringBuilder" "q")))
                  "ToString"))
  ("Monday" "q"))

(deftest direct-receiver-unconvertible-is-type-error
  (list
   (handler-case (dotnet:invoke (the (dotnet "System.String") (%typed-id nil)) "Length")
     (type-error () :type-error))
   (handler-case (dotnet:invoke (the (dotnet "System.String") (%typed-id 42)) "Length")
     (error () :error)))
  (:type-error :error))

(deftest direct-args-match-untyped
  (list
   (%same-typed-untyped (dotnet:static "System.Math" "Max" (the (dotnet "System.Int32") (%typed-id 3))
                                       (the (dotnet "System.Int32") (%typed-id 9)))
                        (dotnet:static "System.Math" "Max" 3 9))
   (%same-typed-untyped (dotnet:static "System.Math" "Max" (the (dotnet "System.Int64") (%typed-id 3))
                                       (the (dotnet "System.Int64") (%typed-id 5000000000)))
                        (dotnet:static "System.Math" "Max" 3 5000000000))
   (%same-typed-untyped (dotnet:static "System.Math" "Max" (the (dotnet "System.Double") (%typed-id 3d0))
                                       (the (dotnet "System.Double") (%typed-id 9.5d0)))
                        (dotnet:static "System.Math" "Max" 3d0 9.5d0))
   (%same-typed-untyped (dotnet:static "System.String" "Concat" (the (dotnet "System.String") (%typed-id "a"))
                                       (the (dotnet "System.String") (%typed-id "b")))
                        (dotnet:static "System.String" "Concat" "a" "b"))
   (%same-typed-untyped (dotnet:static "System.String" "Concat" (the (dotnet "System.Object") (%typed-id "a"))
                                       (the (dotnet "System.Object") (%typed-id 1)))
                        (dotnet:static "System.String" "Concat" "a" 1))
   (%same-typed-untyped (dotnet:static "System.Convert" "ToString" (the (dotnet "System.Boolean") (%typed-id t)))
                        (dotnet:static "System.Convert" "ToString" t))
   (%same-typed-untyped (dotnet:static "System.Char" "IsDigit" (the (dotnet "System.Char") (%typed-id #\5)))
                        (dotnet:static "System.Char" "IsDigit" #\5))
   (%same-typed-untyped (dotnet:invoke (the (dotnet "System.Globalization.DateTimeFormatInfo")
                                            (dotnet:static "System.Globalization.DateTimeFormatInfo" "InvariantInfo"))
                                       "GetDayName"
                                       (the (dotnet "System.DayOfWeek")
                                            (%typed-id (dotnet:static "System.DayOfWeek" "Monday"))))
                        (dotnet:invoke (dotnet:static "System.Globalization.DateTimeFormatInfo" "InvariantInfo")
                                       "GetDayName" (dotnet:static "System.DayOfWeek" "Monday"))))
  (t t t t t t t t))
