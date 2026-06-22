;;; Typed direct-callvirt type inference: a (dotnet:invoke VAR "M" ...)
;;; on a bare local variable lowers to %dotnet-call-direct when VAR's .NET type
;;; is known from its let/let* init form (dotnet:new / dotnet:box / the), with no
;;; explicit THE at the call site. The lowering is purely a speed optimization —
;;; these tests assert the results stay correct, plus that the compiler macro
;;; actually fires (and declines when it must), so a regression to the dynamic
;;; path is still caught behaviorally but a regression in the *inference* is too.

;;; --- behavioral: let-bound receiver, invoked bare (no THE) ---

;; dotnet:new init -> receiver type inferred; zero-arg value-type result.
(deftest infer-new-receiver-length
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" (the (dotnet "System.String") "abcd"))
    (dotnet:invoke sb "get_Length"))
  4)

;; dotnet:new init -> receiver type inferred; reference-type result.
(deftest infer-new-receiver-tostring
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" (the (dotnet "System.String") "hi"))
    (dotnet:invoke sb "ToString"))
  "hi")

;; dotnet:box init -> receiver type inferred (value receiver, virtual ToString).
(deftest infer-box-value-receiver
  (let ((n (dotnet:box 42 "System.Int32")))
    (dotnet:invoke n "ToString"))
  "42")

;; the-init -> receiver type inferred.
(deftest infer-the-receiver
  (let ((sb (the (dotnet "System.Text.StringBuilder")
                 (dotnet:new "System.Text.StringBuilder"))))
    (dotnet:invoke sb "Append" (the (dotnet "System.String") "xy"))
    (dotnet:invoke sb "ToString"))
  "xy")

;; let* binding chained from an earlier dotnet binding: both inferred.
(deftest infer-let*-receiver
  (let* ((sb (dotnet:new "System.Text.StringBuilder"))
         (s (dotnet:box "Z" "System.String")))
    (dotnet:invoke sb "Append" s)
    (dotnet:invoke sb "get_Length"))
  1)

;; Inferred receiver AND inferred bare argument (both bound by the let).
(deftest infer-receiver-and-arg
  (let ((sb (dotnet:new "System.Text.StringBuilder"))
        (s (dotnet:box "abcde" "System.String")))
    (dotnet:invoke sb "Append" s)
    (dotnet:invoke sb "get_Length"))
  5)

;;; --- soundness: shadowing must drop the inferred type ---

;; An inner LET rebinds SB to an untyped value; the inner invoke must NOT use the
;; outer StringBuilder type. Here the inner value is itself a StringBuilder so the
;; result is still well-defined, proving the dynamic path is taken (no miscompile).
(deftest infer-shadow-inner-let
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" (the (dotnet "System.String") "outer"))
    (let ((sb (dotnet:invoke sb "ToString")))     ; sb now a Lisp string
      (concatenate 'string sb "!")))
  "outer!")

;; A bare untyped local (no .NET type source) declines to the dynamic path.
(deftest infer-untyped-local-declines
  (let ((x "plain"))
    (let ((sb (dotnet:new "System.Text.StringBuilder")))
      (dotnet:invoke sb "Append" (the (dotnet "System.String") x))
      (dotnet:invoke sb "ToString")))
  "plain")

;;; --- inference logic, exercised directly (proves the macro fires) ---

;; %dotnet-static-type resolves a bare symbol through the supplied env.
(deftest infer-static-type-symbol
  (let ((r (%dotnet-static-type 'sb '(("SB" . "System.Text.StringBuilder")))))
    (and (consp r)
         (string= (car r) "System.Text.StringBuilder")
         (eq (cdr r) 'sb)))
  t)

;; No env -> a bare symbol is not a static type source.
(deftest infer-static-type-symbol-no-env
  (%dotnet-static-type 'sb nil)
  nil)

;; The compiler macro lowers a bare typed receiver to %dotnet-call-direct.
(deftest infer-cm-lowers-bare-symbol
  (let ((out (%dotnet-invoke-direct-cm
              '(dotnet:invoke sb "ToString")
              '(("SB" . "System.Text.StringBuilder")))))
    (and (consp out)
         (symbolp (car out))
         (string= (symbol-name (car out)) "%DOTNET-CALL-DIRECT")
         (string= (cadr out) "System.Text.StringBuilder")
         (string= (caddr out) "ToString")))
  t)

;; The compiler macro declines (returns the form unchanged) with no type info.
(deftest infer-cm-declines-without-env
  (let ((form '(dotnet:invoke sb "ToString")))
    (eq (%dotnet-invoke-direct-cm form nil) form))
  t)

;;; --- typed-return propagation: method chaining (task 2) ---
;;; The result of a typed invoke whose method returns a *directable* .NET type
;;; (one that marshals to a LispDotNetObject) can itself be the typed receiver of
;;; the next invoke, with no explicit THE. StringBuilder.Append(string) returns the
;;; StringBuilder, so a fluent chain lowers.

;; behavioral: inner Append -> StringBuilder (let-bound receiver, task1 + task2).
(deftest chain-return-let-receiver
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (dotnet:invoke sb "Append" (the (dotnet "System.String") "abc"))
                   "get_Length"))
  3)

;; behavioral: inner receiver typed via THE.
(deftest chain-return-the-receiver
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                                  (the (dotnet "System.String") "wxyz"))
                   "get_Length"))
  4)

;; behavioral: two chained Appends then read length (3-level form).
(deftest chain-return-double-append
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke
     (dotnet:invoke (dotnet:invoke sb "Append" (the (dotnet "System.String") "ab"))
                    "Append" (the (dotnet "System.String") "cde"))
     "ToString"))
  "abcde")

;; %dotnet-method-return-type: directable reference return -> FullName.
(deftest chain-return-type-directable
  (%dotnet-method-return-type "System.Text.StringBuilder" "Append" '("System.String"))
  "System.Text.StringBuilder")

;; %dotnet-method-return-type: String return is natively marshaled -> NIL (decline).
(deftest chain-return-type-string-declines
  (%dotnet-method-return-type "System.Text.StringBuilder" "ToString" nil)
  nil)

;; %dotnet-method-return-type: int return is natively marshaled -> NIL (decline).
(deftest chain-return-type-int-declines
  (%dotnet-method-return-type "System.Text.StringBuilder" "get_Length" nil)
  nil)

;; %dotnet-invoke-return-type: typed chain resolves the inner return type.
(deftest chain-invoke-return-type
  (%dotnet-invoke-return-type
   '(dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                   (the (dotnet "System.String") "x"))
   nil)
  "System.Text.StringBuilder")

;; A chain whose inner returns String does not become a typed-return source.
(deftest chain-invoke-return-type-string-declines
  (%dotnet-invoke-return-type
   '(dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "ToString")
   nil)
  nil)

;; The compiler macro lowers a fluent chain to %dotnet-call-direct on the outer
;; method, with the inner invoke form as the receiver.
(deftest chain-cm-lowers
  (let ((out (%dotnet-invoke-direct-cm
              '(dotnet:invoke
                (dotnet:invoke (the (dotnet "System.Text.StringBuilder") sb) "Append"
                               (the (dotnet "System.String") "x"))
                "get_Length")
              nil)))
    (and (consp out)
         (string= (symbol-name (car out)) "%DOTNET-CALL-DIRECT")
         (string= (cadr out) "System.Text.StringBuilder")
         (string= (caddr out) "get_Length")))
  t)

;;; %merge-disjoint-locals must track the RECV/ARG locals carried inside a
;;; :dotnet-call-direct-locals op (typed dotnet:invoke lowering). Those locals
;;; live in nested lists, invisible to the slot-share liveness scan, so an earlier
;;; short-lived LispObject local freed a slot the scan reused for a DARG — renaming
;;; the DARG's declare/stloc while the nested op reference stayed DARG_n, yielding
;;; "Undeclared local: DARG_n" at assembly time. Compiling this body (3 dead locals
;;; before a typed invoke with a typed arg) must succeed and run.
(defun i316-typed-invoke-after-dead-locals (n)
  (let ((a (princ-to-string n))) (length a))
  (let ((b (princ-to-string n))) (length b))
  (let ((c (princ-to-string n))) (length c))
  (dotnet:invoke (dotnet:new "System.Text.StringBuilder") "Append"
                 (dotnet:box n "System.Int32")))

(deftest i316-merge-locals-keeps-typed-invoke-args
  (format nil "~a" (dotnet:invoke (i316-typed-invoke-after-dead-locals 41) "ToString"))
  "41")
