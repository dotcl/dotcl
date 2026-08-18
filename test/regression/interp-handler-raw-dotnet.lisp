;;; An interpreted HANDLER-CASE / HANDLER-BIND must catch raw .NET exceptions.
;;;
;;; The runtime throws plain .NET exceptions in many places (throw new
;;; ArgumentException and friends). A .NET throw never goes through SIGNAL, and a
;;; handler cluster is only consulted BY signal — so nothing connected the two
;;; unless something wrapped the body in a try/catch.
;;;
;;; Compiled code has that try/catch: COMPILE-HANDLER-CASE emits one and converts
;;; what it catches. The tree-walk evaluator had none, so
;;;
;;;   (handler-case (dotcl::%throw-raw-clr-exception) (error (c) :caught))
;;;
;;; returned :CAUGHT compiled and flew past the handler interpreted. It was easy
;;; to miss because an enclosing COMPILED handler-case catches the escapee, so the
;;; condition still looks handled from outside — the divergence only shows when
;;; the handler-case itself is the interpreted one.
;;;
;;; The fix: %MINI-EVAL now implements HANDLER-BIND itself instead of letting the
;;; macro expansion stand in for it, the same way COMPILE-FORM consults its
;;; handler table before macroexpanding. The macro still expands to a body inline
;;; in a PROGN, which is what a code walker needs to see; that shape just is not
;;; what evaluates the form any more. The evaluator uses
;;; %CALL-WITH-HANDLER-CLUSTER, which runs the body under a catch that converts a
;;; raw exception into a Lisp condition. The conversion is what dispatches:
;;; LispErrorException signals in its constructor, and that happens inside the
;;; catch, where the cluster this call pushed is still on the stack.
;;;
;;; Thunking the body in the MACRO would have worked too, and is wrong: a code
;;; walker cannot cross a function boundary, so every walker would pay for an
;;; evaluator problem. HANDLER-BIND-BODY-INLINE-NOT-THUNKED in recent-fixes.lisp
;;; asserts the inline shape, and is what caught the attempt.

(defmacro %ihr (mode form)
  `(let ((dotcl:*evaluator-mode* ,mode))
     (handler-case (eval ,form)
       (error (e) (list :escaped (type-of e))))))

;;; --- the raw .NET exception is caught by an interpreted handler

(defparameter %ihr-hc
  '(handler-case (dotcl::%throw-raw-clr-exception)
    (error (c) (list :caught (type-of c)))))

(deftest-compiled-only interp-handler-raw-dotnet.handler-case-compile
  (%ihr :compile %ihr-hc)
  (:caught program-error))

(deftest interp-handler-raw-dotnet.handler-case-interpret
  (%ihr :interpret %ihr-hc)
  (:caught program-error))

;;; The source really is a raw .NET throw, not a Lisp condition that would have
;;; been caught anyway: the condition carries the CLR exception type.
;;;
;;; The source is DOTCL::%THROW-RAW-CLR-EXCEPTION, which exists for this test.
;;; Earlier versions used whatever runtime function happened to throw a raw
;;; exception at the time -- MAKE-HASH-TABLE with a bad :weakness, then the
;;; printer radix check -- and each one broke this file the day that bug was
;;; fixed. Worse than breaking: the file would keep passing while testing
;;; nothing, if the replacement source had stopped being raw unnoticed. A
;;; deliberate thrower is the only source that stays raw on purpose.
(deftest interp-handler-raw-dotnet.source-is-really-raw
  (%ihr :interpret '(handler-case (dotcl::%throw-raw-clr-exception)
                     (error (c) (let ((ty (dotnet:exception-type c)))
                                  (and ty (search "ArgumentException"
                                                  (princ-to-string ty))
                                       t)))))
  t)

;;; --- HANDLER-BIND: transfer, and decline

(defparameter %ihr-hb-transfer
  '(catch 'out
    (handler-bind ((error (lambda (c) (throw 'out (list :caught (type-of c))))))
      (dotcl::%throw-raw-clr-exception))))

(deftest-compiled-only interp-handler-raw-dotnet.handler-bind-transfer-compile
  (%ihr :compile %ihr-hb-transfer)
  (:caught program-error))

(deftest interp-handler-raw-dotnet.handler-bind-transfer-interpret
  (%ihr :interpret %ihr-hb-transfer)
  (:caught program-error))

;;; A handler that returns normally declines. The condition then propagates —
;;; it cannot resume at the throw point, because by the time a .NET exception is
;;; catchable those frames are gone. The flag proves the handler did run.
(defparameter %ihr-hb-decline
  '(let ((ran nil))
    (list (handler-case
              (handler-bind ((error (lambda (c) (declare (ignore c)) (setq ran t))))
                (dotcl::%throw-raw-clr-exception))
            (error () :propagated))
          ran)))

(deftest-compiled-only interp-handler-raw-dotnet.handler-bind-decline-compile
  (%ihr :compile %ihr-hb-decline)
  (:propagated t))

(deftest interp-handler-raw-dotnet.handler-bind-decline-interpret
  (%ihr :interpret %ihr-hb-decline)
  (:propagated t))

;;; --- over-fix guards -----------------------------------------------------
;;; Catching MORE than the compiled evaluator would be just as wrong as catching
;;; nothing, so each case is asserted for both.

;;; a clause whose type does not match must not catch
(deftest-compiled-only interp-handler-raw-dotnet.type-must-match-compile
  (%ihr :compile '(handler-case (dotcl::%throw-raw-clr-exception) (type-error () :wrong)))
  (:escaped program-error))

(deftest interp-handler-raw-dotnet.type-must-match-interpret
  (%ihr :interpret '(handler-case (dotcl::%throw-raw-clr-exception) (type-error () :wrong)))
  (:escaped program-error))

;;; UNWIND-PROTECT cleanup runs on the way out
(deftest interp-handler-raw-dotnet.unwind-protect-runs-interpret
  (%ihr :interpret '(let ((c nil))
                     (list (handler-case
                               (unwind-protect (dotcl::%throw-raw-clr-exception)
                                 (setq c :cleaned))
                             (error () :caught))
                           c)))
  (:caught :cleaned))

;;; the innermost handler-case wins
(deftest interp-handler-raw-dotnet.innermost-wins-interpret
  (%ihr :interpret '(handler-case
                     (handler-case (dotcl::%throw-raw-clr-exception) (error () :inner))
                     (error () :outer)))
  :inner)

;;; ordinary Lisp conditions are unaffected
(deftest interp-handler-raw-dotnet.lisp-error-still-caught-interpret
  (%ihr :interpret '(handler-case (error "x") (error (c) (list :caught (type-of c)))))
  (:caught simple-error))

(deftest interp-handler-raw-dotnet.lisp-error-handler-bind-interpret
  (%ihr :interpret '(catch 'o (handler-bind ((error (lambda (c) (throw 'o (type-of c)))))
                                (error "y"))))
  simple-error)

;;; a body that does not signal returns its values, including several
(deftest interp-handler-raw-dotnet.body-value-interpret
  (%ihr :interpret '(handler-case (+ 1 2) (error () :no)))
  3)

(deftest interp-handler-raw-dotnet.body-multiple-values-interpret
  (%ihr :interpret '(multiple-value-list (handler-case (values 1 2 3) (error () :no))))
  (1 2 3))

(deftest interp-handler-raw-dotnet.no-error-clause-interpret
  (%ihr :interpret '(handler-case (values 1 2) (error () :no) (:no-error (a b) (list :ok a b))))
  (:ok 1 2))

;;; control transfers out of a HANDLER-BIND body are not conditions and must pass
;;; through the new catch untouched
(deftest interp-handler-raw-dotnet.return-from-body-interpret
  (%ihr :interpret '(block b (handler-bind ((error #'identity)) (return-from b :left))))
  :left)

(deftest interp-handler-raw-dotnet.throw-from-body-interpret
  (%ihr :interpret '(catch 'tg (handler-bind ((error #'identity)) (throw 'tg :thrown))))
  :thrown)

(deftest interp-handler-raw-dotnet.go-from-body-interpret
  (%ihr :interpret '(let ((r nil))
                     (tagbody
                        (handler-bind ((error #'identity)) (go done))
                        (setq r :not-skipped)
                      done)
                     r))
  nil)

;;; a lexical macro of the same name still shadows the special handling — the
;;; evaluator checks the lexical macro environment before its own form table
(deftest interp-handler-raw-dotnet.macrolet-shadows-interpret
  (%ihr :interpret '(macrolet ((handler-bind (bindings &body body)
                                 (declare (ignore bindings body))
                                 :shadowed))
                     (handler-bind ((error #'identity)) (error "never runs"))))
  :shadowed)
