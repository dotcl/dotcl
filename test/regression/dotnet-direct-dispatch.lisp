;;; Typed direct-callvirt codegen — direct dispatch for a known-typed dotnet:invoke.
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
  ;; Receiver via THE, argument via BOX — both recognized.
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
