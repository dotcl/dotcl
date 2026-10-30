;;; Standard objects inside a logical block share its sections (SBCL 2.6.8).

(defun %pnss-print (thunk margin &key (miser nil) (length nil) (pretty t))
  (let ((*print-right-margin* margin)
        (*print-pretty* pretty)
        (*print-escape* t)
        (*print-readably* nil)
        (*print-length* length)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-circle* nil)
        (*print-miser-width* miser))
    (funcall thunk)))

(deftest pprint-nested-standard-sections.no-outer-newline
  (%pnss-print
   (lambda ()
     (with-output-to-string (s)
       (pprint-logical-block (s '(a (b c) d (e (f g)) h) :prefix "(" :suffix ")")
         (dotimes (i 6)
           (write (pprint-pop) :stream s :pretty t)
           (write-string " " s)))))
   24)
  "(A (B C) D (E
            (F
             G)) H NIL )")

(deftest pprint-nested-standard-sections.trailing-space
  (%pnss-print
   (lambda ()
     (with-output-to-string (s)
       (pprint-logical-block (s '(a (b (c d)) e f) :prefix "(" :suffix ")")
         (loop (pprint-exit-if-list-exhausted)
               (write (pprint-pop) :stream s :pretty t)
               (write-string " " s)
               (pprint-newline :linear s)))))
   8)
  "(A
 (B
  (C
   D))
 E
 F
 )")

(deftest pprint-nested-standard-sections.fill
  (%pnss-print
   (lambda ()
     (with-output-to-string (s)
       (pprint-logical-block (s '((a b) (c d) (e f)) :prefix "(" :suffix ")")
         (loop (pprint-exit-if-list-exhausted)
               (write (pprint-pop) :stream s :pretty t)
               (write-string " " s)
               (pprint-newline :fill s)))))
   16 :miser 10)
  "((A B) (C D)
 (E F) )")

(deftest pprint-nested-standard-sections.mandatory
  (%pnss-print
   (lambda ()
     (with-output-to-string (s)
       (pprint-logical-block (s '(x (y z) (w (v (u))) t) :prefix "(" :suffix ")")
         (loop (pprint-exit-if-list-exhausted)
               (write (pprint-pop) :stream s :pretty t)
               (write-string " " s)
               (pprint-newline :mandatory s)))))
   12 :miser 80)
  "(X
 (Y Z)
 (W
  (V (U)))
 T
 )")

(deftest pprint-nested-standard-sections.print-length
  (%pnss-print
   (lambda ()
     (with-output-to-string (s)
       (pprint-logical-block (s '(a (b c) d (e (f g)) h) :prefix "(" :suffix ")")
         (dotimes (i 6)
           (write (pprint-pop) :stream s :pretty t)
           (write-string " " s)))))
   24 :length 5)
  "(A (B C) D (E
            (F
             G)) H ...)")

(deftest pprint-nested-standard-sections.list-printer-fill
  (%pnss-print
   (lambda () (with-output-to-string (s) (pprint-fill s '((a b) (c d) (e f)) nil)))
   5)
  #.(format nil "(A~% B)~%(C~% D)~%(E F)"))

(deftest pprint-nested-standard-sections.list-printer-linear
  (%pnss-print
   (lambda () (with-output-to-string (s) (pprint-linear s '(a (b (c d)) e f) t)))
   5)
  #.(format nil "(A~% (B~%  (C~%   D))~% E~% F)"))

(deftest pprint-nested-standard-sections.list-printer-tabular
  (%pnss-print
   (lambda () (with-output-to-string (s) (pprint-tabular s '(a (b (c d)) e f) t 2)))
   8)
  #.(format nil "(A~% (B~%  (C~%   D))~% E~% F)"))
