;;; Multi-line input in the bundled REPL.
;;;
;;; A form is not a line, so the editor holds the whole of one and decides for
;;; itself when it is finished: Enter sends a form whose parentheses are closed
;;; and opens an indented line inside one that is not. Everything that decides
;;; is a function of a string and answers without a terminal, which is what is
;;; checked here -- the counting, the indentation it leads to, the delimiters a
;;; terminal wraps pasted text in, moving between the lines of one form, and
;;; the redraw of an input that has more than one line in it.

(require "dotcl-repl")

;;; Escape and any non-ASCII character are spelled out, so an expected value
;;; can be written as a plain readable literal and a failure prints something a
;;; reader can compare by eye. The same spelling as the single-line drawing
;;; tests use, so the two files read alike.
(defun rml-show (s)
  (with-output-to-string (out)
    (loop for i below (length s)
          for ch = (char s i)
          do (cond ((char= ch #\Escape) (write-string "<ESC>" out))
                   ((or (< (char-code ch) 32) (> (char-code ch) 126))
                    (format out "<U+~4,'0X>" (char-code ch)))
                   (t (write-char ch out))))))

(defun rml-render (prompt-width content point width cursor-row)
  "RENDER with its output made printable. Row numbers pass through."
  (multiple-value-bind (out row rows)
      (dotcl-repl::render prompt-width content point width cursor-row)
    (values (rml-show out) row rows)))

(defun rml-csi-finals (s)
  "The final byte of every control sequence in S, each one once."
  (let ((finals '())
        (n (length s)))
    (loop for i below (1- n)
          when (and (char= (char s i) #\Escape) (char= (char s (1+ i)) #\[))
            do (let ((j (+ i 2)))
                 (loop while (and (< j n)
                                  (or (digit-char-p (char s j))
                                      (char= (char s j) #\;)))
                       do (incf j))
                 (when (< j n) (pushnew (char s j) finals))))
    (sort finals #'char<)))

(defun rml-text (&rest lines)
  "LINES joined with newlines, so a buffer of several lines reads as its lines."
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (write-char #\Newline out))
             (write-string line out))))

;;; HIRAGANA LETTER A: two columns wide on a terminal, one character in the
;;; buffer. Built from its code point so this file stays ASCII.
(defparameter *rml-wide* (string (code-char #x3042)))

;;; -- Counting parentheses ----------------------------------------------------

(deftest rml-nothing-is-balanced
  (dotcl-repl::paren-depth "")
  0)

(deftest rml-a-closed-form-is-balanced
  (dotcl-repl::paren-depth "(+ 1 2)")
  0)

(deftest rml-an-open-form-has-depth
  (dotcl-repl::paren-depth "(defun f (x)")
  1)

(deftest rml-three-levels-of-nesting
  (dotcl-repl::paren-depth "(a (b (c")
  3)

(deftest rml-three-levels-closed-again
  (dotcl-repl::paren-depth "(a (b (c)))")
  0)

;;; A bracket too many is not an unfinished form, and counting it as one would
;;; leave no way to send the line. It goes to the reader, which says so.
(deftest rml-over-closing-goes-negative
  (dotcl-repl::paren-depth "(+ 1 2))")
  -1)

;;; -- What the scan steps over ------------------------------------------------

(deftest rml-parens-in-a-string-do-not-count
  (dotcl-repl::paren-depth "(format t \"((( \")")
  0)

(deftest rml-a-string-can-hide-the-only-open-paren
  (dotcl-repl::paren-depth "(format t \"(\"")
  1)

;;; An escaped quote does not end the string, so the bracket after it is still
;;; inside one.
(deftest rml-escaped-quote-does-not-end-the-string
  (dotcl-repl::paren-depth "(f \"\\\"(\")")
  0)

(deftest rml-a-string-holding-only-an-escaped-quote
  (dotcl-repl::paren-depth "(f \"\\\"\")")
  0)

;;; A backslash before the closing quote is an escaped backslash, and the quote
;;; after it does close the string.
(deftest rml-escaped-backslash-lets-the-string-close
  (dotcl-repl::paren-depth "(f \"\\\\\")")
  0)

(deftest rml-a-semicolon-comment-is-skipped
  (dotcl-repl::paren-depth "(a ; ((((")
  1)

(deftest rml-a-semicolon-comment-ends-at-its-line
  (dotcl-repl::paren-depth (rml-text "(a ; ))))" "  b)"))
  0)

(deftest rml-a-quote-inside-a-comment-is-not-a-string
  (dotcl-repl::paren-depth (rml-text "(a ; \" (" "  b)"))
  0)

(deftest rml-a-block-comment-is-skipped
  (dotcl-repl::paren-depth "(a #| ( ( ( |# b)")
  0)

(deftest rml-block-comments-nest
  (dotcl-repl::paren-depth "(a #| ( #| ( |# ( |# b)")
  0)

;;; A block comment left open swallows the rest, so the brackets after it are
;;; not counted either.
(deftest rml-an-unclosed-block-comment-swallows-the-rest
  (dotcl-repl::paren-depth "(a #| ) ) )")
  1)

(deftest rml-a-block-comment-can-span-lines
  (dotcl-repl::paren-depth (rml-text "(a #| (" "   ( |# b)"))
  0)

(deftest rml-an-open-paren-character-is-a-character
  (dotcl-repl::paren-depth "(list #\\()")
  0)

(deftest rml-a-close-paren-character-is-a-character
  (dotcl-repl::paren-depth "(list #\\))")
  0)

;;; #\" is the double quote character and does not open a string, so the
;;; bracket after it is counted.
(deftest rml-a-quote-character-does-not-open-a-string
  (dotcl-repl::paren-depth "(list #\\\" (")
  2)

;;; A named character is three characters and then letters, which count as
;;; nothing either way.
(deftest rml-a-named-character-is-harmless
  (dotcl-repl::paren-depth "(list #\\Space)")
  0)

;;; The count can be taken of a prefix, which is how the indentation finds out
;;; what one line did.
(deftest rml-a-prefix-can-be-counted
  (dotcl-repl::paren-depth "(a (b))" 4)
  2)

;;; -- Indentation -------------------------------------------------------------

;;; Typing a form a line at a time: each line after the first opens with the
;;; blanks AUTO-INDENT gives it, and the text is what the buffer holds at the
;;; end. The expected values are the examples the rules were agreed on.
(defun rml-type-lines (&rest lines)
  (let ((text (first lines)))
    (dolist (line (rest lines) text)
      (setf text
            (concatenate 'string text (string #\Newline)
                         (make-string (dotcl-repl::auto-indent text (length text))
                                      :initial-element #\Space)
                         line)))))

(deftest rml-example-defun
  (rml-type-lines "(defun add1 (x)" "(+ x 1))")
  #.(rml-text "(defun add1 (x)"
              "  (+ x 1))"))

(deftest rml-example-let
  (rml-type-lines "(let ((x 1)" "(y 2))" "(+ x y))")
  #.(rml-text "(let ((x 1)"
              "      (y 2))"
              "  (+ x y))"))

;;; The clauses after the first go two columns in and not under the first:
;;; the known price of having no table of operators.
(deftest rml-example-cond
  (rml-type-lines "(cond ((foo)" "(bar))" "(t (baz)))")
  #.(rml-text "(cond ((foo)"
              "       (bar))"
              "  (t (baz)))"))

(deftest rml-example-format
  (rml-type-lines "(format t \"~a\" (list 1" "2))")
  #.(rml-text "(format t \"~a\" (list 1"
              "                     2))"))

;;; Rule 2: a bracket that is the first thing on its line indents its body by
;;; two.
(deftest rml-an-opened-form-indents-by-two
  (dotcl-repl::auto-indent "(defun f (x)" 12)
  2)

(deftest rml-an-indented-line-head-bracket-indents-by-two
  (dotcl-repl::auto-indent (rml-text "(defun f (x)" "  (let ((a 1))") 27)
  4)

;;; Rule 1: a bracket after something else on its line lines up with its
;;; first argument.
(deftest rml-mid-line-bracket-lines-up-with-the-first-argument
  (dotcl-repl::auto-indent (rml-text "(defun f (x)" "  (if (plusp x") 27)
  13)

;;; Rule 1 with nothing after the operator: one past the bracket.
(deftest rml-mid-line-bracket-with-only-an-operator
  (dotcl-repl::auto-indent "(foo (bar" 9)
  6)

;;; Rule 1 with nothing at all after the bracket.
(deftest rml-mid-line-bracket-with-nothing-after-it
  (dotcl-repl::auto-indent "(foo (" 6)
  6)

;;; A list as the operator slot is one element, however many brackets it has.
(deftest rml-a-list-in-the-operator-slot-is-one-element
  (dotcl-repl::auto-indent "(x ((b c) d" 11)
  10)

(deftest rml-quoted-and-string-elements-are-elements
  (dotcl-repl::auto-indent "(x (f '(1 2) \"s\"" 16)
  6)

;;; Three levels, the innermost one mid-line: it is the one that decides.
(deftest rml-three-levels-the-innermost-decides
  (dotcl-repl::auto-indent "(a (b (c d" 10)
  9)

;;; The innermost closes and the middle one is left open: back to its first
;;; argument.
(deftest rml-three-levels-back-to-the-middle
  (dotcl-repl::auto-indent (rml-text "(a (b x (c d" "         e)") 24)
  6)

;;; A bracket in a string is not a bracket, so it neither opens nor is lined
;;; up with.
(deftest rml-a-bracket-in-a-string-is-not-lined-up-with
  (dotcl-repl::auto-indent "(f \"(\" (g" 9)
  8)

(deftest rml-a-bracket-in-a-string-does-not-indent
  (dotcl-repl::auto-indent "(f \"(((\")" 9)
  0)

(deftest rml-a-commented-bracket-does-not-indent
  (dotcl-repl::auto-indent "(f x) ; (((" 11)
  0)

;;; A character that is a bracket is an element, not a bracket.
(deftest rml-a-bracket-character-is-an-element
  (dotcl-repl::auto-indent "(a (list #\\( x" 14)
  9)

;;; Rule 3: a line that closes what it opens keeps its indentation.
(deftest rml-a-line-that-opens-nothing-keeps-its-indent
  (dotcl-repl::auto-indent (rml-text "(defun f (x)" "  (print x)") 24)
  2)

(deftest rml-a-balanced-line-under-a-first-argument-stays-there
  (dotcl-repl::auto-indent (rml-text "(list 1" "      2") 15)
  6)

;;; A line that begins with a closing bracket closes into the lines above,
;;; and the bracket still open decides.
(deftest rml-a-line-starting-with-a-close-paren
  (dotcl-repl::auto-indent (rml-text "(a (b c" "      d" ")") 17)
  2)

;;; A line that closes the whole form: nothing is open, so column zero.
(deftest rml-a-line-closing-the-form-goes-to-zero
  (dotcl-repl::auto-indent (rml-text "(defun f (x)" "    (print x))") 27)
  0)

(deftest rml-indent-never-goes-below-zero
  (dotcl-repl::auto-indent "(a))))" 6)
  0)

(deftest rml-a-fresh-line-indents-by-nothing
  (dotcl-repl::auto-indent "" 0)
  0)

;;; A newline typed in the middle of a line: only what is before the cursor
;;; counts.
(deftest rml-only-text-before-the-cursor-counts
  (dotcl-repl::auto-indent "(foo (bar baz qux)" 9)
  6)

;;; The stack of open brackets, innermost first.
(deftest rml-open-brackets-innermost-first
  (dotcl-repl::open-brackets "(a (b) (c \"(\" (d")
  (14 7 0))

;;; -- What Enter means --------------------------------------------------------

(deftest rml-a-closed-form-is-sent
  (dotcl-repl::enter-action "(+ 1 2)")
  :submit)

(deftest rml-an-open-form-opens-a-line
  (dotcl-repl::enter-action "(defun f (x)")
  :open-line)

(deftest rml-an-over-closed-form-is-sent
  (dotcl-repl::enter-action "(+ 1 2))")
  :submit)

(deftest rml-an-empty-line-is-sent
  (dotcl-repl::enter-action "")
  :submit)

;;; A command is never a form. Counting its brackets would leave ,help ( with
;;; no way to finish, waiting for a bracket the command was never going to
;;; read.
(deftest rml-a-command-with-an-open-bracket-is-sent
  (dotcl-repl::enter-action ",help (")
  :submit)

(deftest rml-an-ordinary-command-is-sent
  (dotcl-repl::enter-action ",help")
  :submit)

;;; On a continuation line the leading comma is an unquote in a backquoted form
;;; typed across several lines, so it is counted like anything else.
(deftest rml-a-leading-comma-is-a-form-on-a-continuation-line
  (dotcl-repl::enter-action ",help (" :commands nil)
  :open-line)

(deftest rml-a-closed-comma-form-is-still-sent-on-a-continuation-line
  (dotcl-repl::enter-action ",(list 1 2)" :commands nil)
  :submit)

;;; Inside a paste the text is arriving from somewhere else all at once, and a
;;; line of it that happens to balance is not the reader being asked for
;;; anything.
(deftest rml-a-newline-inside-a-paste-opens-a-line
  (dotcl-repl::enter-action "(+ 1 2)" :pasting t)
  :open-line)

(deftest rml-a-command-inside-a-paste-opens-a-line
  (dotcl-repl::enter-action ",help" :pasting t)
  :open-line)

;;; -- Paste delimiters --------------------------------------------------------

(deftest rml-paste-start-is-recognised
  (dotcl-repl::classify-csi "200" #\~)
  :paste-start)

(deftest rml-paste-end-is-recognised
  (dotcl-repl::classify-csi "201" #\~)
  :paste-end)

(deftest rml-another-tilde-sequence-is-ignored
  (dotcl-repl::classify-csi "5" #\~)
  :ignored)

;;; The parameters alone are not enough: 200 with another final byte is some
;;; other sequence entirely.
(deftest rml-the-final-byte-has-to-be-a-tilde
  (dotcl-repl::classify-csi "200" #\R)
  :ignored)

(deftest rml-an-empty-sequence-is-ignored
  (dotcl-repl::classify-csi "" #\A)
  :ignored)

(deftest rml-the-mode-is-turned-on-by-2004h
  (rml-show dotcl-repl::*bracketed-paste-on*)
  "<ESC>[?2004h")

(deftest rml-the-mode-is-turned-off-by-2004l
  (rml-show dotcl-repl::*bracketed-paste-off*)
  "<ESC>[?2004l")

;;; -- Lines and the cursor between them ---------------------------------------

(deftest rml-line-start-of-the-first-line
  (dotcl-repl::line-start (rml-text "abc" "de") 2)
  0)

(deftest rml-line-start-of-the-second-line
  (dotcl-repl::line-start (rml-text "abc" "de") 5)
  4)

(deftest rml-line-end-of-the-first-line
  (dotcl-repl::line-end (rml-text "abc" "de") 1)
  3)

(deftest rml-line-end-of-the-last-line
  (dotcl-repl::line-end (rml-text "abc" "de") 5)
  6)

(deftest rml-no-row-above-the-first
  (dotcl-repl::previous-row-point (rml-text "abc" "de") 2)
  nil)

;;; The column is kept: two characters into the second line is two characters
;;; into the first.
(deftest rml-moving-up-keeps-the-column
  (dotcl-repl::previous-row-point (rml-text "abcd" "ef") 7)
  2)

;;; And where the row above is too short for it, the end of that row.
(deftest rml-moving-up-stops-at-the-end-of-a-short-row
  (dotcl-repl::previous-row-point (rml-text "ab" "cdefg") 8)
  2)

(deftest rml-no-row-below-the-last
  (dotcl-repl::next-row-point (rml-text "abc" "de") 5)
  nil)

(deftest rml-moving-down-keeps-the-column
  (dotcl-repl::next-row-point (rml-text "abcd" "efgh") 2)
  7)

(deftest rml-moving-down-stops-at-the-end-of-a-short-row
  (dotcl-repl::next-row-point (rml-text "abcd" "e") 3)
  6)

;;; -- Where a newline lands ---------------------------------------------------

;;; Mid-row, a newline ends the row and the next one opens INDENT columns in.
(deftest rml-newline-ends-the-row-it-is-on
  (dotcl-repl::newline-column 9 20 8)
  28)

;;; Text that filled the last cell of a row has already put the column on the
;;; next one, and the newline leaves it there: a line filling the screen
;;; exactly is followed by the next line, not by a blank row.
(deftest rml-newline-after-a-full-row-adds-no-row
  (dotcl-repl::newline-column 20 20 8)
  28)

;;; With no prompt to line up under, a newline at the very start still opens a
;;; row.
(deftest rml-newline-at-column-zero-opens-a-row
  (dotcl-repl::newline-column 0 20 0)
  20)

(deftest rml-layout-counts-a-newline-and-its-indent
  (dotcl-repl::layout-column 8 (rml-text "ab" "cd") 20 5 8)
  30)

;;; -- Drawing more than one line ----------------------------------------------

;;; Two lines under an eight-column prompt. The newline is written as a
;;; carriage return and a line feed, and the second row is then padded out to
;;; the width of the prompt so the lines start under each other. That padding
;;; is on the screen only: the buffer holds the newline and nothing else.
(deftest rml-two-line-buffer
  (rml-render 8 (rml-text "(a" " b)") 6 20 0)
  "<ESC>[G<ESC>[8C<ESC>[J(a<U+000D><U+000A>         b)<ESC>[G<ESC>[11C" 1 2)

;;; The same two lines with the cursor on the first of them: the text is
;;; written out in full and the cursor then climbs back a row.
(deftest rml-cursor-on-the-first-of-two-lines
  (rml-render 8 (rml-text "(a" " b)") 1 20 0)
  "<ESC>[G<ESC>[8C<ESC>[J(a<U+000D><U+000A>         b)<ESC>[1A<ESC>[G<ESC>[9C" 0 2)

;;; A continuation line long enough to wrap. Twelve of the twenty characters
;;; fit beside the padding and the other eight go on a third row.
(deftest rml-continuation-line-wraps
  (rml-render 8 (rml-text "a" (make-string 20 :initial-element #\x)) 22 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Ja<U+000D><U+000A>        xxxxxxxxxxxxxxxxxxxx<ESC>[G<ESC>[8C"
  2 3)

;;; A wide character on a continuation line, in the one cell it cannot fit in.
;;; The terminal leaves that cell blank and starts the character on the row
;;; below, and the arithmetic makes the same choice.
(deftest rml-wide-char-on-a-continuation-line-does-not-straddle
  (rml-render 11 (rml-text "a" *rml-wide*) 3 12 0)
  "<ESC>[G<ESC>[11C<ESC>[Ja<U+000D><U+000A>           <U+3042><ESC>[G<ESC>[2C"
  2 3)

;;; An ordinary wide character on a continuation line is two columns, counted
;;; from the padding.
(deftest rml-wide-char-on-a-continuation-line
  (rml-render 8 (rml-text "a" (concatenate 'string *rml-wide* "b")) 4 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Ja<U+000D><U+000A>        <U+3042>b<ESC>[G<ESC>[11C" 1 2)

;;; A line that ends exactly on the margin followed by a newline. The carriage
;;; return and line feed the newline is written as are the same two characters
;;; that force a pending wrap, so one pair does for both and the next line
;;; starts on the row the wrap had already reached.
(deftest rml-newline-after-a-row-that-ends-on-the-margin
  (rml-render 8 (rml-text (make-string 12 :initial-element #\x) "y") 14 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Jxxxxxxxxxxxx<U+000D><U+000A>        y<ESC>[G<ESC>[9C" 1 2)

;;; Three lines, and a redraw that starts from the middle one.
(deftest rml-three-lines-from-the-middle-row
  (rml-render 4 (rml-text "(a" "(b" "c))") 8 20 1)
  "<ESC>[1A<ESC>[G<ESC>[4C<ESC>[J(a<U+000D><U+000A>    (b<U+000D><U+000A>    c))<ESC>[G<ESC>[6C"
  2 3)

;;; Drawing several lines still uses nothing but the four relative movements:
;;; up, right, column one, erase below. No absolute positioning and no cursor
;;; report, the same guarantee the single-line drawing gives.
(deftest rml-multi-line-uses-only-relative-movement
  (rml-csi-finals (dotcl-repl::render 8 (rml-text "(a" " b)") 1 20 0))
  (#\A #\C #\G #\J))

(deftest rml-multi-line-never-asks-for-the-cursor-position
  (search "<ESC>[6n" (rml-render 8 (rml-text "(a" " b)") 1 20 1))
  nil)

;;; An empty line in the middle of a form is still a row.
(deftest rml-an-empty-line-is-still-a-row
  (nth-value 2 (rml-render 8 (rml-text "(a" "" "b)") 6 20 0))
  3)
