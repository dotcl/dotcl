;;; Syntax highlighting of the input line in the bundled REPL.
;;;
;;; Strings, comments and keywords are painted by the redraw, with the same
;;; spans as the matching bracket. Where they are is found by a scan that
;;; follows the bracket count, and what is painted follows the REPL's colour
;;; decision. Both are functions of their arguments.

(require "dotcl-repl")

(defun rsh (text)
  (dotcl-repl::syntax-ranges text))

(defun rsh-esc (s)
  (with-output-to-string (out)
    (loop for ch across s
          do (case ch
               (#\Escape (write-string "<E>" out))
               (#\Return (write-string "<R>" out))
               (#\Newline (write-string "<N>" out))
               (t (write-char ch out))))))

;;; Nothing to paint in plain code.
(deftest rsh-plain
  (rsh "(+ 1 2)")
  nil)

;;; A string, with its quotes.
(deftest rsh-string
  (rsh "(f \"ab\" x)")
  ((3 7 :string)))

;;; A backslash in a string takes the next character, quote included.
(deftest rsh-string-escaped-quote
  (rsh "(f \"a\\\"b\" x)")
  ((3 9 :string)))

;;; A string still open runs to the end, across lines.
(deftest rsh-string-open
  (rsh (format nil "(f \"ab~%cd"))
  ((3 9 :string)))

;;; A semicolon comment runs to the end of its line and no further.
(deftest rsh-line-comment
  (rsh (format nil "(f ; hi~% :k)"))
  ((3 7 :comment) (9 11 :keyword)))

;;; A block comment, nested, and one still open.
(deftest rsh-block-comment
  (list (rsh "a #| x #| y |# z |# b") (rsh "a #| x"))
  (((2 19 :comment)) ((2 6 :comment))))

;;; Keywords: at the start, after a bracket, after a quote; up to a delimiter.
(deftest rsh-keywords
  (rsh ":a (f :bb) ':c")
  ((0 2 :keyword) (6 9 :keyword) (12 14 :keyword)))

;;; A colon inside a token is a package prefix, and #: is not a keyword.
(deftest rsh-not-keywords
  (rsh "(cl:car x) #:g pkg::y")
  nil)

;;; What is in a string or a comment is not a keyword, and a quote or a
;;; semicolon inside a string ends nothing.
(deftest rsh-nested-kinds
  (rsh "\":a ; b\" ; \"c :d")
  ((0 8 :string) (9 16 :comment)))

;;; #\" and #\; are characters.
(deftest rsh-character-literals
  (rsh "(list #\\\" #\\; :k)")
  ((14 16 :keyword)))

;;; A lone colon is a (one character) keyword token; an empty text is nothing.
(deftest rsh-edges
  (list (rsh ":") (rsh ""))
  (((0 1 :keyword)) nil))

;;; The roles exist.
(deftest rsh-paint-roles
  (mapcar (lambda (role) (rsh-esc (dotcl::%repl-paint role "x" t)))
          '(:string :comment :keyword))
  ("<E>[32mx<E>[0m" "<E>[90mx<E>[0m" "<E>[35mx<E>[0m"))

;;; Here standard output is not painted: no spans of either kind.
(deftest rsh-no-spans-without-colour
  (list (dotcl-repl::syntax-spans "(f \"a\" :k ; c)")
        (dotcl-repl::input-spans "(f \"a\" :k)" 10))
  (nil nil))

;;; The on switch is exported and on by default.
(deftest rsh-default-on
  dotcl-repl:*syntax-highlight*
  t)

;;; The redraw with a string span and the matching-bracket span together, as
;;; the editor combines them: sorted, painted in order.
(deftest rsh-render-combined
  (let ((on-s (format nil "~C[32m" #\Escape))
        (on-m (format nil "~C[7m" #\Escape)))
    (rsh-esc (dotcl-repl::render 0 "(f \"a\")" 7 80 0
                                 (list (list 0 1 on-m) (list 3 6 on-s)))))
  "<E>[G<E>[J<E>[7m(<E>[0mf <E>[32m\"a\"<E>[0m)<E>[G<E>[7C")
