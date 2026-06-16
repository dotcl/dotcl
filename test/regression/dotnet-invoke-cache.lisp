;;; Regression tests for the dotnet:invoke / dotnet:static MethodInfo cache.
;;; The cache must never change overload resolution or results: it keys on the arg
;;; runtime types, falls back to InvokeMember for anything it does not cache, and
;;; serves the same MethodInfo on repeat (hot-loop) calls.

;; Append(string) and Append(int) are distinct overloads; selection keys on the arg
;; runtime type, and the repeated Append(string) must hit the cached overload.
(deftest invoke-cache-append-overloads
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" "a")          ; Append(string)
    (dotnet:invoke sb "Append" 42)           ; Append(int) -> "42"
    (dotnet:invoke sb "Append" "a")          ; cached Append(string)
    (dotnet:invoke sb "ToString"))
  "a42a")

;; Hot loop through the cached fast path stays correct.
(deftest invoke-cache-hot-loop
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotimes (i 1000) (dotnet:invoke sb "Append" "x"))
    (dotnet:invoke sb "get_Length"))
  1000)

;; Same (type, name) on two objects but different arg runtime types -> different
;; keys -> different overloads; no stale string->int (or int->string) hit.
(deftest invoke-cache-distinct-arg-types
  (let ((s1 (dotnet:new "System.Text.StringBuilder"))
        (s2 (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke s1 "Append" 7)            ; Append(int) -> "7"
    (dotnet:invoke s2 "Append" "7")          ; Append(string) -> "7"
    (list (dotnet:invoke s1 "ToString")
          (dotnet:invoke s2 "ToString")))
  ("7" "7"))

;; Static path cache: Math.Abs(int) repeated, then a third value through the hit.
(deftest invoke-cache-static-abs
  (list (dotnet:static "System.Math" "Abs" -5)
        (dotnet:static "System.Math" "Abs" -5)
        (dotnet:static "System.Math" "Abs" -3))
  (5 5 3))

;; Omitted C# optional parameters (#24) still resolve: the cache returns false for
;; the arg-count mismatch and the InvokeMember + optional-default retry handles it.
(deftest invoke-cache-optional-fallthrough
  (dotnet:invoke (dotnet:new "DotCL.TestSupport.OptionalArgs") "Greet")
  "hi world;")

;; A fully-supplied call on a method that *has* optional params is cacheable and
;; must still produce the right result.
(deftest invoke-cache-optional-all-supplied
  (dotnet:invoke (dotnet:new "DotCL.TestSupport.OptionalArgs") "Greet" "Bob" 2)
  "hi Bob;hi Bob;")
