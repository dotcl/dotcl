;;; Pretty printing paths that used to lay out a nested object without the
;;; text that follows it: a logical block under *PRINT-PRETTY* NIL, FORMAT's
;;; ~A with a queued tab, and a user pprint-dispatch function's buffer.
;;; Expected values are SBCL 2.6.8's output.

(defun %g1c-ppd-cons (stream object)
  (write-string "[" stream)
  (pprint-fill stream object)
  (write-string "]" stream))

(defun %g1c-ppd-logical (stream object)
  (pprint-logical-block (stream object :prefix "{" :suffix "}")
    (loop (pprint-exit-if-list-exhausted)
          (write (pprint-pop) :stream stream :pretty t)
          (pprint-newline :linear stream))))

(defun %g1c-ppd-symbol (stream object)
  (declare (ignore object))
  (write-string "<SYMBOL>" stream))

(defun %g1c-ppd-default (stream object)
  (declare (ignore object))
  (write-string "<DEFAULT>" stream))

(defun %g1c-ppd-member (stream object)
  (declare (ignore object))
  (write-string "<MEMBER>" stream))

(defun %g1c-ppd-string (stream object)
  (declare (ignore object))
  (write-string "<STRING>" stream))

(deftest pprint-g1c.not-pretty-outer-block
  (let ((*print-right-margin* 12) (*print-miser-width* nil) (*print-pretty* nil))
    (with-output-to-string (s)
      (pprint-logical-block (s '(a (b (c d)) e f) :prefix "(" :suffix ")")
        (loop (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream s :pretty t)
              (write-string " " s)
              (pprint-newline :miser s)))))
  #.(format nil "(A (B~%    (C~%     D)) E F )"))

(deftest pprint-g1c.format-tab-after-object
  (let ((*print-right-margin* 12) (*print-miser-width* 0) (*print-pretty* t))
    (format nil "~@<~a~10T~a~:>"
            '((a b) (c d) (e f))
            '(x (y z) (w (v (u))) t)))
  #.(format nil "((A B)~% (C D)~% (E~%  F))     (X~%           (Y~%            Z)~%           (W~%            (V~%             (U)))~%           T)"))

(deftest pprint-g1c.format-line-relative-tab-after-object
  (let ((*print-right-margin* 40) (*print-miser-width* 0) (*print-pretty* t))
    (format nil "~@<~a~5,3T~a~:>"
            '("string one" "string two" "string three")
            nil))
  #.(format nil "(string one string two~% string three)   NIL"))

(deftest pprint-g1c.dispatch-cons-fill
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t) (*print-right-margin* 20))
    (set-pprint-dispatch t '%g1c-ppd-default 0)
    (set-pprint-dispatch 'symbol '%g1c-ppd-symbol 1)
    (set-pprint-dispatch '(member foo) '%g1c-ppd-member 5)
    (set-pprint-dispatch 'cons '%g1c-ppd-cons 2)
    (prin1-to-string '(foo bar)))
  #.(format nil "[(<MEMBER>~%  <SYMBOL>)]"))

(deftest pprint-g1c.dispatch-nested-logical-block
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t) (*print-right-margin* 20))
    (set-pprint-dispatch t '%g1c-ppd-default 0)
    (set-pprint-dispatch 'symbol '%g1c-ppd-symbol 1)
    (set-pprint-dispatch '(member foo) '%g1c-ppd-member 5)
    (set-pprint-dispatch 'cons '%g1c-ppd-cons 2)
    (set-pprint-dispatch '(cons (eql zork)) '%g1c-ppd-logical 6)
    (prin1-to-string '((zork 1) (foo))))
  #.(format nil "[({<SYMBOL>~%   <DEFAULT>~%   }~%  [(<MEMBER>)])]"))

(deftest pprint-g1c.dispatch-string-inside-cons
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t) (*print-right-margin* 20))
    (set-pprint-dispatch 'cons '%g1c-ppd-cons 2)
    (set-pprint-dispatch 'string '%g1c-ppd-string 3)
    (prin1-to-string '("a" "b")))
  #.(format nil "[(<STRING>~%  <STRING>)]"))

;;; The block now exists under *PRINT-PRETTY* NIL, but its newlines stay inert.
(deftest pprint-g1c.printers-flat-under-not-pretty
  (let ((*print-pretty* nil) (*print-right-margin* 10))
    (list (with-output-to-string (s)
            (pprint-fill s '(aaaa bbbb cccc dddd)))
          (with-output-to-string (s)
            (pprint-linear s '(aaaa bbbb cccc dddd)))
          (with-output-to-string (s)
            (pprint-tabular s '(aaaa bbbb cccc dddd) nil 8))))
  ("(AAAA BBBB CCCC DDDD)"
   "(AAAA BBBB CCCC DDDD)"
   "AAAA BBBB CCCC DDDD"))
