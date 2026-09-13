;;; ~^ with nothing around it to terminate ends the FORMAT operation.
;;;
;;; CLHS 22.3.9.2: ~^ terminates the innermost ~{...~} or ~<...~>, and when there
;;; is none it terminates the whole format operation, producing what has been
;;; written so far.
;;;
;;; dotcl implements ~^ by throwing a control-flow exception that the iteration
;;; and justification directives catch. Nothing caught it at the top, so a ~^ that
;;; actually fired outside them escaped FORMAT and surfaced as a PROGRAM-ERROR:
;;; (format nil "~a~^b" 1) signalled instead of returning "1". The same applied to
;;; a ~^ inside a directive that does not catch it, such as ~[...~].
;;;
;;; Expected values are SBCL's.

;;; ~^ does nothing while arguments remain.
(deftest format-up-and-out.does-not-fire-with-arguments-left
  (list (format nil "a~^b" 1)
        (format nil "~1^ab")
        (format nil "~{~a~^,~}" '(1 2)))
  ("ab" "ab" "1,2"))

;;; ~^ fires: the operation ends, and what was produced before it is the result.
(deftest format-up-and-out.fires-at-top-level
  (list (format nil "a~^b")
        (format nil "~a~^b" 1)
        (format nil "a~0^b"))
  ("a" "1" "a"))

;;; A directive that does not catch ~^ lets it reach the top, where it now ends
;;; the operation rather than escaping as an error.
(deftest format-up-and-out.fires-inside-a-non-catching-directive
  (format nil "~[~^a~;b~]" 0)
  "")

;;; It is a normal return, not a condition: nothing to handle.
(deftest format-up-and-out.is-not-an-error
  (handler-case (list (format nil "a~^b") (format nil "~a~^b" 1))
    (error () :error))
  ("a" "1"))

;;; The directives that do catch ~^ still catch it -- the top-level catch must not
;;; take over from them.
(deftest format-up-and-out.iteration-still-catches
  (list (format nil "~{~a~^,~}" '(1 2 3))
        (format nil "~{~a~^,~}" '())
        (format nil "~{~a~^,~}" '(1)))
  ("1,2,3" "" "1"))

;;; FORMAT to a stream goes through the same top, and writes the partial output.
(deftest format-up-and-out.to-a-stream
  (with-output-to-string (s)
    (format s "a~^b")
    (format s "|")
    (format s "~a~^b" 1))
  "a|1")
