;;; A FORMAT directive that needs an argument and has none is an error.
;;;
;;; CLHS 22.3 makes FORMAT an error when the arguments run out. dotcl printed
;;; nothing for the directive and carried on, so a control string with the wrong
;;; argument count produced quietly incomplete output -- worst where FORMAT is
;;; building the text of another error, which is exactly where nobody is looking.
;;; An unrecognised directive was echoed back verbatim for the same reason.
;;;
;;; Running out is NOT an error for the directives that use it as their stopping
;;; condition: ~^ terminates, ~{...~} ends its iteration, ~#[ counts what is left
;;; on purpose, and ~@{ takes whatever remains.
;;;
;;; Expected values are SBCL's, taken by running the same 32 probes on both.

(defmacro %fma-try (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

;;; The directives that print an argument.

(deftest format-missing-argument.printing-directives
  (list (%fma-try (format nil "~a")) (%fma-try (format nil "~s"))
        (%fma-try (format nil "~d")) (%fma-try (format nil "~b"))
        (%fma-try (format nil "~o")) (%fma-try (format nil "~x"))
        (%fma-try (format nil "~8r")) (%fma-try (format nil "~c"))
        (%fma-try (format nil "~f")) (%fma-try (format nil "~e"))
        (%fma-try (format nil "~g")) (%fma-try (format nil "~$"))
        (%fma-try (format nil "~p")) (%fma-try (format nil "~:p"))
        (%fma-try (format nil "~w")))
  (:error :error :error :error :error :error :error :error
   :error :error :error :error :error :error :error))

;;; Running out part-way through, which is the shape a wrong argument count has.

(deftest format-missing-argument.runs-out-partway
  (list (%fma-try (format nil "~a~a" 1))
        (%fma-try (format nil "~a~a~a" 1 2))
        (%fma-try (format nil "prefix ~a suffix")))
  (:error :error :error))

;;; An unrecognised directive.

(deftest format-missing-argument.unknown-directive
  (list (%fma-try (format nil "~Q" 1))
        (%fma-try (format nil "~!")))
  (:error :error))

;;; Directives that do not need an argument keep working with none.

(deftest format-missing-argument.no-argument-directives
  (list (char (format nil "~%") 0) (format nil "~&") (format nil "~~")
        (format nil "~t") (format nil "~(abc~)") (format nil "~10<ab~>"))
  (#\newline "" "~" " " "abc" "        ab"))

;;; Exhaustion is the stopping condition for these, not an error.

(deftest format-missing-argument.exhaustion-is-not-an-error
  (list (format nil "~#[a~;b~]")
        (format nil "~@{~a~}")
        (format nil "~{~a~^,~}" '())
        (format nil "a~^b")
        (format nil "~a~^b" 1))
  ("a" "" "" "a" "1"))

;;; Enough arguments: unchanged.
