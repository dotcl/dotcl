;;; Per-line prefixes of enclosing logical blocks.
;;;
;;; A new line inside a logical block starts with the per-line prefix of every
;;; enclosing block, each at the column where it was printed on the first line
;;; (XP; the expected strings are SBCL 2.6.8's). Two paths used to lose them:
;;;
;;; 1. A nested block had no per-line prefix of its own, so a newline inside it
;;;    was followed only by indentation spaces. Nested directly, through ~/, or
;;;    through ~A of a condition whose report opens its own ~@<...~:>.
;;; 2. ~& and ~% (and FRESH-LINE on the stream) wrote a bare newline.
;;;
;;; A per-line prefix printed at a column other than 0 is also indented to that
;;; column on the following lines now.

(defun pnplp-inner (s &rest r)
  (declare (ignore r))
  (format s "~@<The number was~@:_~S.~@:>" 1))

(defun pnplp-cause ()
  (make-condition 'simple-error
                  :format-control "~@<The number was~@:_~S.~@:>"
                  :format-arguments '(1)))

(deftest pnplp-nested-direct
  (format nil "~@<> ~@;~@<The number was~@:_~S.~@:>~:>" 1)
  "> The number was
> 1.")

(deftest pnplp-nested-slash
  (format nil "~@<> ~@;~/pnplp-inner/~:>" nil)
  "> The number was
> 1.")

(deftest pnplp-nested-condition
  (format nil "~@<> ~@;~A~:>" (pnplp-cause))
  "> The number was
> 1.")

(deftest pnplp-prefix-at-column
  (princ-to-string (make-condition 'simple-error
                                   :format-control "x~@<> ~@;~A~:>"
                                   :format-arguments (list (pnplp-cause))))
  "x> The number was
 > 1.")

(deftest pnplp-fresh-line-directive
  (format nil "~@<| ~@;Mock Error.~&See also~:>")
  "| Mock Error.
| See also")

(deftest pnplp-newline-directive
  (list (format nil "~@<| ~@;a~%b~:>")
        (format nil "~@<(~;a~%b~;)~:>")
        ;; ~& right after ~% is not at column 0: the prefix is there (SBCL too)
        (string= (format nil "~@<| ~@;a~%~&b~:>")
                 (format nil "| a~%| ~%| b")))
  ("| a
| b"
   "(a
b)"
   t))

(deftest pnplp-fill-then-fresh-line
  (format nil "~@<| ~@;A~@[ ~:_Caused by:~&~@<> ~@;~A~@:>~]~:>" "x")
  "| A Caused by:
| > x")

(deftest pnplp-staged-text-column
  (list (format nil "~@<> ~@;x ~@<a~@:_b~:>~:>")
        (format nil "~@<| ~@;a ~@<> ~@;b~@:_c~:>~:>")
        (format nil "x~@<> ~@;a~@:_b~:>"))
  ("> x a
>   b"
   "| a > b
|   > c"
   "x> a
 > b"))

(deftest pnplp-macro-blocks
  (list (with-output-to-string (s)
          (write-string "xy" s)
          (pprint-logical-block (s nil :per-line-prefix ";; ")
            (write-string "a" s)
            (pprint-newline :mandatory s)
            (pprint-logical-block (s nil :prefix "(" :suffix ")")
              (write-string "b" s)
              (pprint-newline :mandatory s)
              (write-string "c" s))))
        (with-output-to-string (s)
          (pprint-logical-block (s nil :per-line-prefix ";; ")
            (write-string "a" s)
            (pprint-logical-block (s nil)
              (terpri s)
              (write-string "b" s))))
        (with-output-to-string (s)
          (pprint-logical-block (s nil :per-line-prefix ";; ")
            (write-string "a" s)
            (fresh-line s)
            (write-string "b" s))))
  ("xy;; a
  ;; (b
  ;;  c)"
   ";; a
;; b"
   ";; a
;; b"))
