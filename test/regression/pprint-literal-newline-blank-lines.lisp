;;; A line the program ended itself (TERPRI, FRESH-LINE, a newline character) keeps what is on it,
;;; a prefix or an indentation included; a conditional newline takes the blanks off. *PRINT-LINES*
;;; adds its " .." after a line of blanks as it stands. Expected strings are SBCL's.

(defun %lit (thunk &key (margin 40) (lines nil) (miser nil))
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* margin) (*print-lines* lines) (*print-miser-width* miser))
      (funcall thunk s))))
(defun %lit-cases ()
  (list
   (%lit (lambda (s) (pprint-logical-block (s nil :prefix "(" :suffix ")") (write-string "ab   " s) (terpri s) (write-string "cd" s))))
   (%lit (lambda (s) (pprint-logical-block (s nil :prefix "(" :suffix ")") (write-string "ab   " s) (write-char #\Newline s) (write-string "cd" s))))
   (%lit (lambda (s) (pprint-logical-block (s nil :per-line-prefix "  ") (write-string "ab" s) (terpri s) (terpri s) (write-string "cd" s))))
   (%lit (lambda (s) (pprint-logical-block (s nil :prefix "(" :suffix ")") (write-string "ab   " s) (pprint-newline :mandatory s) (write-string "cd" s))))
   (%lit (lambda (s) (pprint-logical-block (s nil :per-line-prefix "  ") (write-string "ab" s) (pprint-newline :mandatory s) (pprint-newline :mandatory s) (write-string "cd" s))))
   ;; 03: a line that is only the per-line prefix, ended by FRESH-LINE after a miser break
   (%lit (lambda (s) (pprint-logical-block (s nil :per-line-prefix "    ") (write-string "yy" s) (write-char #\Space s) (pprint-newline :miser s) (fresh-line s) (write-string "yy" s))) :margin 7 :miser 40)
   ;; 04: *print-lines* after a line that is only the indentation
   (%lit (lambda (s) (pprint-logical-block (s '("2" "g") :prefix "<" :suffix "LONG-SUFFIX-HERE") (write (pprint-pop) :stream s) (write-char #\Space s) (pprint-newline :linear s) (terpri s) (write (pprint-pop) :stream s))) :margin 80 :lines 2 :miser 0)
   ;; *print-lines* cut after text and a conditional newline: the blanks before it go
   (%lit (lambda (s) (pprint-logical-block (s nil :prefix "(" :suffix ")") (write-string "aaa" s) (pprint-newline :mandatory s) (write-string "bbb" s) (pprint-newline :mandatory s) (write-string "ccc" s))) :lines 2)
   (%lit (lambda (s) (pprint-logical-block (s nil :prefix "(" :suffix ")") (write-string "aaa   " s) (pprint-newline :mandatory s) (write-string "bbb" s) (pprint-newline :mandatory s) (write-string "ccc" s))) :lines 2)))

(deftest pprint-literal-newline-blank-lines.cases
  (%lit-cases)
  ("(ab   
cd)" "(ab   
cd)" "  ab
  
  cd" "(ab
 cd)" "  ab

  cd" "    yy
    
    yy" "<\"2\"
  ..LONG-SUFFIX-HERE" "(aaa
 bbb ..)" "(aaa
 bbb ..)"))
