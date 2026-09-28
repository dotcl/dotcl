;;; Line drawing in the bundled REPL.
;;;
;;; The editor used to ask the terminal where the cursor was before every
;;; redraw. On Unix that question is a Device Status Report written to the
;;; terminal whose answer comes back as ordinary standard input, so anything
;;; else reading the same stream could swallow it and leave the reply in the
;;; user's line. The redraw now uses relative movement and keeps its own idea
;;; of where the cursor is, which means the whole layout is arithmetic and can
;;; be checked here without a terminal.

(require "dotcl-repl")

;;; Escape and any non-ASCII character are spelled out, so an expected value
;;; can be written as a plain readable literal and a failure prints something
;;; a reader can compare by eye.
(defun rr-show (s)
  (with-output-to-string (out)
    (loop for i below (length s)
          for ch = (char s i)
          do (cond ((char= ch #\Escape) (write-string "<ESC>" out))
                   ((or (< (char-code ch) 32) (> (char-code ch) 126))
                    (format out "<U+~4,'0X>" (char-code ch)))
                   (t (write-char ch out))))))

(defun rr-render (prompt-width content point width cursor-row)
  "RENDER with its output made printable. Row numbers pass through."
  (multiple-value-bind (out row rows)
      (dotcl-repl::render prompt-width content point width cursor-row)
    (values (rr-show out) row rows)))

;;; The final byte of a control sequence says which operation it is. Collecting
;;; them is how these tests assert that nothing but the four allowed movements
;;; ever reaches the terminal.
(defun rr-csi-finals (s)
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

;;; HIRAGANA LETTER A: two columns wide on a terminal, one character in the
;;; buffer. Built from its code point so this file stays ASCII.
(defparameter *rr-wide* (string (code-char #x3042)))

;;; An empty buffer still redraws: back to the prompt row, out past the prompt,
;;; erase whatever the last draw left, and stop where the first character would
;;; go. One row, cursor on it.
(deftest rr-empty-buffer
  (rr-render 8 "" 0 80 0)
  "<ESC>[G<ESC>[8C<ESC>[J<ESC>[G<ESC>[8C" 0 1)

;;; A short line with the point at its end.
(deftest rr-short-line-point-at-end
  (rr-render 8 "abc" 3 80 0)
  "<ESC>[G<ESC>[8C<ESC>[Jabc<ESC>[G<ESC>[11C" 0 1)

;;; The point in the middle: the rightward move is measured from the prompt, so
;;; it is the prompt width plus the width of the text before the point.
(deftest rr-point-in-middle
  (rr-render 8 "hello" 2 80 0)
  "<ESC>[G<ESC>[8C<ESC>[Jhello<ESC>[G<ESC>[10C" 0 1)

;;; The same with a wide character before the point. Two characters precede it
;;; but they occupy three columns, so the move is 11 and not 10.
(deftest rr-point-past-wide-char
  (rr-render 8 (concatenate 'string *rr-wide* "ab") 2 80 0)
  "<ESC>[G<ESC>[8C<ESC>[J<U+3042>ab<ESC>[G<ESC>[11C" 0 1)

;;; A wide character is two columns.
(deftest rr-wide-char-is-two-columns
  (dotcl-repl::layout-column 0 *rr-wide* 80)
  2)

;;; With one cell left before the margin a narrow character fills it.
(deftest rr-narrow-char-fills-last-cell
  (dotcl-repl::layout-column 9 "a" 10)
  10)

;;; A wide character cannot: the terminal leaves that cell blank and starts the
;;; character on the next row, so the column jumps by three rather than two.
(deftest rr-wide-char-does-not-straddle-margin
  (dotcl-repl::layout-column 9 *rr-wide* 10)
  12)

;;; The same seen through a redraw: the character lands on the second row and
;;; the cursor comes to rest two columns in, not one.
(deftest rr-straddling-wide-char-redraw
  (rr-render 9 *rr-wide* 1 10 0)
  "<ESC>[G<ESC>[9C<ESC>[J<U+3042><ESC>[G<ESC>[2C" 1 2)

;;; A line long enough to wrap: 8 columns of prompt and 30 of text on a
;;; 20-column terminal end at column 38, which is row 1 column 18.
(deftest rr-wrapped-line-point-at-end
  (rr-render 8 (make-string 30 :initial-element #\x) 30 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Jxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx<ESC>[G<ESC>[18C" 1 2)

;;; Same line, point still on the first row: the cursor has to climb back up
;;; one row after the text is written.
(deftest rr-wrapped-line-point-on-first-row
  (rr-render 8 (make-string 30 :initial-element #\x) 5 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Jxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx<ESC>[1A<ESC>[G<ESC>[13C" 0 2)

;;; Text that ends exactly on the margin leaves the terminal holding a pending
;;; wrap, with the cursor still on the row it just filled. The redraw forces the
;;; wrap with a carriage return and a newline so the screen matches the count.
(deftest rr-deferred-wrap-is-forced
  (rr-render 8 (make-string 12 :initial-element #\x) 12 20 0)
  "<ESC>[G<ESC>[8C<ESC>[Jxxxxxxxxxxxx<U+000D><U+000A><ESC>[G" 1 2)

;;; Shrinking. The previous draw took two rows and left the cursor on the
;;; second; a short line climbs back to the prompt row, erases everything below
;;; it, and reports one row.
(deftest rr-shrinking-erases-below
  (rr-render 8 "abcde" 5 20 1)
  "<ESC>[1A<ESC>[G<ESC>[8C<ESC>[Jabcde<ESC>[G<ESC>[13C" 0 1)

;;; Only the four relative movements are ever emitted: up, right, column one,
;;; erase below. No absolute positioning (final byte H) and no cursor report.
(deftest rr-uses-only-relative-movement
  (rr-csi-finals (dotcl-repl::render 8 (make-string 30 :initial-element #\x)
                                     5 20 1))
  (#\A #\C #\G #\J))

(deftest rr-never-asks-for-the-cursor-position
  (search "<ESC>[6n" (rr-render 8 "abc" 1 20 1))
  nil)

;;; A prompt as wide as the terminal is degenerate: a rightward move stops at
;;; the margin, so the model stops there too rather than counting past it.
(deftest rr-prompt-wider-than-terminal-is-clamped
  (rr-render 40 "" 0 20 0)
  "<ESC>[G<ESC>[19C<ESC>[J<ESC>[G<ESC>[19C" 0 1)
