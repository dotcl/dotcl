;;; The control-structure directives need their arguments too.
;;;
;;; The printing directives were made to signal on exhaustion earlier; ~[ ~{ ~*
;;; and ~? were left out because each shares its case with a form that
;;; legitimately takes no argument -- ~#[ reads the remaining count, ~@{ takes
;;; whatever is left, ~@* goes to an absolute position. Getting that wrong in
;;; either direction is easy, so both halves are pinned here: what must signal,
;;; and what must not.
;;;
;;; ~* is not "is there an argument" but "does the new position stay inside the
;;; list": walking off either end was silent, so (format nil "~*") with nothing
;;; to skip, or ~2@* past the end, carried on against arguments that were not
;;; there.
;;;
;;; Expected values are SBCL's, from the same 25 probes on both.

(defmacro %fcd-try (form)
  `(handler-case ,form (error () :error)))

;;; Must signal: the argument is genuinely required.

(deftest format-control-directive-args.conditional-needs-an-argument
  (list (%fcd-try (format nil "~[a~;b~]"))
        (%fcd-try (format nil "~:[a~;b~]"))
        (%fcd-try (format nil "~@[x~]")))
  (:error :error :error))

(deftest format-control-directive-args.iteration-needs-its-list
  (list (%fcd-try (format nil "~{~a~}"))
        (%fcd-try (format nil "~:{~a~}")))
  (:error :error))

(deftest format-control-directive-args.goto-must-stay-in-range
  (list (%fcd-try (format nil "~*"))
        (%fcd-try (format nil "~2*" 1))
        (%fcd-try (format nil "~:*"))
        (%fcd-try (format nil "~2@*" 1)))
  (:error :error :error :error))

(deftest format-control-directive-args.recursive-needs-its-control-string
  (list (%fcd-try (format nil "~?"))
        (%fcd-try (format nil "~@?")))
  (:error :error))

;;; Must NOT signal: these forms take no argument by design, and the checks above
;;; sit next to them.

(deftest format-control-directive-args.forms-that-take-no-argument
  (list (format nil "~#[z~;o~]")
        (format nil "~@{~a~}")
        (format nil "~:@{~a~}")
        (format nil "~@*")
        (format nil "~{~a~}" nil)
        (format nil "~@[~a~]" nil))
  ("z" "" "" "" "" ""))

;;; And the ordinary uses still work, including the ones that move the pointer.

(deftest format-control-directive-args.ordinary-uses
  (list (format nil "~[a~;b~]" 0)
        (format nil "~:[a~;b~]" nil)
        (format nil "~*" 1)
        (format nil "~a~:*~a" 1)
        (format nil "~?" "~a" '(1))
        (format nil "~@?" "~a" 1)
        (format nil "~@?" "x"))
  ("a" "a" "" "11" "1" "1" "x"))
