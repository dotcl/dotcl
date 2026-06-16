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
