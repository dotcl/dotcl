;;; The matching bracket in the bundled REPL.
;;;
;;; With the cursor just past a closing bracket, the line editor paints the
;;; opening bracket it closes, on the same line or on a line above. The
;;; bracket is found by the scan that decides whether Enter sends the form, so
;;; brackets in strings, comments and #\( do not count, and it is shown by the
;;; redraw painting that one character. Both halves are functions of their
;;; arguments and are checked here without a terminal.

(require "dotcl-repl")

(defun rpm-esc (s)
  "S with ESC spelled as <E>, CR as <R> and LF as <N>, for readable expectations."
  (with-output-to-string (out)
    (loop for ch across s
          do (case ch
               (#\Escape (write-string "<E>" out))
               (#\Return (write-string "<R>" out))
               (#\Newline (write-string "<N>" out))
               (t (write-char ch out))))))

(defun rpm-match (text &optional (point (length text)))
  (dotcl-repl::matching-open text point))

(defparameter *rpm-on* (format nil "~C[7m" #\Escape))

(defun rpm-render (prompt-width content point width cursor-row spans)
  (multiple-value-bind (out row rows)
      (dotcl-repl::render prompt-width content point width cursor-row spans)
    (values (rpm-esc out) row rows)))

;;; On one line, the innermost and the outermost.
(deftest rpm-same-line
  (list (rpm-match "(a (b) c)" 6) (rpm-match "(a (b) c)" 9))
  (3 0))

;;; Nested closers, each finding its own opener.
(deftest rpm-nested
  (list (rpm-match "(((x)))" 5) (rpm-match "(((x)))" 6) (rpm-match "(((x)))" 7))
  (2 1 0))

;;; A form typed over several lines: the opener is on the first line.
(deftest rpm-multi-line
  (rpm-match (format nil "(defun f (x)~%  (+ x 1))"))
  0)

(deftest rpm-multi-line-inner
  (let ((text (format nil "(defun f (x)~%  (+ x 1))")))
    (rpm-match text (1- (length text))))
  15)

;;; A closer inside a string is not a bracket, and does not upset the count
;;; for the one after the string.
(deftest rpm-string
  (list (rpm-match "(a \")\")" 5) (rpm-match "(a \")\")" 7))
  (nil 0))

;;; An escaped quote does not end the string.
(deftest rpm-string-escaped-quote
  (list (rpm-match "(a \"\\\")\")" 7) (rpm-match "(a \"\\\")\")" 9))
  (nil 0))

;;; #\) is a character, and #\( does not open anything.
(deftest rpm-character-literal
  (list (rpm-match "(a #\\))" 6) (rpm-match "(a #\\))" 7)
        (rpm-match "(a #\\( b)" 9))
  (nil 0 0))

;;; A closer in a semicolon comment, and the one on the line after it.
(deftest rpm-line-comment
  (let ((text (format nil "(a ; )~%)")))
    (list (rpm-match text 6) (rpm-match text 8)))
  (nil 0))

;;; A closer in a block comment.
(deftest rpm-block-comment
  (list (rpm-match "(a #| ) |# )" 7) (rpm-match "(a #| ) |# )" 12))
  (nil 0))

;;; Nothing to match: a stray closer, a point not after a closer, the start.
(deftest rpm-nothing
  (list (rpm-match ")") (rpm-match "a)") (rpm-match "(a)" 2) (rpm-match "(a)" 0)
        (rpm-match "" 0) (rpm-match "(a" 2))
  (nil nil nil nil nil nil))

;;; A stray closer earlier in the text does not shift the match of a later one.
(deftest rpm-after-stray
  (rpm-match ") (a)")
  2)

;;; The paint itself: the role exists and is reverse video.
(deftest rpm-paint-role
  (list (rpm-esc (dotcl::%repl-paint :match "(" t))
        (dotcl::%repl-paint :match "(" nil))
  ("<E>[7m(<E>[0m" "("))

;;; Here standard output is not painted, as it is not for a pipe, a file or
;;; --color=never: no span, so the redraw writes nothing it did not before.
(deftest rpm-no-spans-without-colour
  (list (dotcl-repl::paint-prefix :match) (dotcl-repl::match-spans "(a)" 3))
  (nil nil))

;;; One painted character: ON before it, a reset after it, and the cursor
;;; moves exactly as without the paint.
(deftest rpm-render-one-char
  (rpm-render 8 "(a)" 3 80 0 (list (list 0 1 *rpm-on*)))
  "<E>[G<E>[8C<E>[J<E>[7m(<E>[0ma)<E>[G<E>[11C" 0 1)

;;; Painted up to the last character: the reset still comes before the cursor
;;; moves back.
(deftest rpm-render-last-char
  (rpm-render 8 "(a)" 3 80 0 (list (list 2 3 *rpm-on*)))
  "<E>[G<E>[8C<E>[J(a<E>[7m)<E>[0m<E>[G<E>[11C" 0 1)

;;; The opener on the row above: painted where it is, and the rows and the
;;; resting place of the cursor are those of the unpainted redraw.
(deftest rpm-render-row-above
  (let* ((text (format nil "(f~% x)"))
         (spans (list (list 0 1 *rpm-on*))))
    (multiple-value-bind (plain prow prows)
        (dotcl-repl::render 8 text (length text) 80 1)
      (multiple-value-bind (out row rows)
          (dotcl-repl::render 8 text (length text) 80 1 spans)
        (list (rpm-esc out)
              (= row prow) (= rows prows)
              (string= plain
                       (let ((s out))
                         (loop for seq in (list *rpm-on* (format nil "~C[0m" #\Escape))
                               do (loop for at = (search seq s)
                                        while at
                                        do (setf s (concatenate 'string
                                                                (subseq s 0 at)
                                                                (subseq s (+ at (length seq)))))))
                         s))))))
  ("<E>[1A<E>[G<E>[8C<E>[J<E>[7m(<E>[0mf<R><N>         x)<E>[G<E>[11C" t t t))

;;; A span across a newline: the newline and the padding after it are not
;;; painted, and the paint starts again on the next row.
(deftest rpm-render-span-over-newline
  (rpm-render 2 (format nil "\"a~%b\"") 0 80 0 (list (list 0 5 *rpm-on*)))
  "<E>[G<E>[2C<E>[J<E>[7m\"a<E>[0m<R><N>  <E>[7mb\"<E>[0m<E>[1A<E>[G<E>[2C" 0 2)

;;; Two spans next to each other with different paints: a reset between them.
(deftest rpm-render-adjacent-spans
  (rpm-render 0 "ab" 2 80 0 (list (list 0 1 "<1>") (list 1 2 "<2>")))
  "<E>[G<E>[J<1>a<E>[0m<2>b<E>[0m<E>[G<E>[2C" 0 1)

;;; A long line that wraps: the paint does not change where the rows break.
(deftest rpm-render-wrap-unchanged
  (let ((text (make-string 30 :initial-element #\a)))
    (multiple-value-bind (a arow arows) (dotcl-repl::render 4 text 30 10 0)
      (declare (ignore a))
      (multiple-value-bind (b brow brows)
          (dotcl-repl::render 4 text 30 10 0 (list (list 0 30 *rpm-on*)))
        (declare (ignore b))
        (list arow brow arows brows))))
  (3 3 4 4))
