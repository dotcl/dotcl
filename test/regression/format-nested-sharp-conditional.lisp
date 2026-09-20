;;; A ~[ nested inside a ~[ has to close itself, not the one around it.
;;;
;;; The scanner that finds the end of a conditional stepped over a directive's
;;; prefix parameters by skipping digits, and a prefix parameter is also # (the
;;; number of arguments left), v, 'c or a sign. So in ~#[ the scanner read the #
;;; as the whole directive and the [ as literal text, never counted the nesting,
;;; and let the inner ~] close the outer conditional. Whatever followed was then
;;; read outside any block, which is where "unknown directive ~}" came from.
;;;
;;; The string below is 3d-vectors', which is what found this: it lists the
;;; components of a swizzle, with "and" before the last one.

(deftest format-sharp-conditional-in-conditional
  (format nil "~#[a~:;[~#[~;!~:;,~]]~]" 'x 'y 'z)
  "[,]")

(deftest format-sharp-conditional-in-iteration-in-conditional
  (format nil "~#[a~:;~@{~#[!~:;,~]~}~]" 'x 'y 'z)
  ",")

(deftest format-sharp-conditional-nested-english-list
  (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}" '(x y z))
  "X, Y, and Z")

(deftest format-sharp-conditional-nested-one-element
  (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}" '(x))
  "X")

(deftest format-sharp-conditional-nested-two-elements
  (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}" '(x y))
  "X and Y")

;;; A v prefix parameter reaches the same scanner.

(deftest format-v-conditional-in-conditional
  (format nil "~#[a~:;[~v[zero~;one~:;many~]]~]" 1 'y)
  "[one]")

;;; And the clause separator still knows which ~; was ~:; -- that flag came from
;;; the same piece of parsing.

(deftest format-sharp-conditional-default-clause-still-found
  (list (format nil "~#[none~;one~:;many~]")
        (format nil "~#[none~;one~:;many~]" 'x)
        (format nil "~#[none~;one~:;many~]" 'x 'y))
  ("none" "one" "many"))

(deftest format-conditional-without-default-selects-nothing
  (format nil "[~#[none~;one~]]" 'x 'y)
  "[]")
