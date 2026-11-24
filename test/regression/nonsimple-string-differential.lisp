;;; Non-simple string ranges and printer keywords, expected lines from SBCL.

(defpackage :probe-pkg (:use))
(dolist (name '("HELLO" "hello world" "MiXeD" "A(B" "123" "CL-USER-SYMBOL"))
  (intern name :probe-pkg))

(defpackage :nss-test (:use :cl))
(in-package :nss-test)

(defun codes-string (s)
  (with-output-to-string (o)
    (let ((first t))
      (loop for c across s do
        (if first (setf first nil) (write-char #\, o))
        (format o "~a" (char-code c))))))

(defun enc (x)
  (cond
    ((null x) "NIL")
    ((eq x t) "T")
    ((integerp x) (format nil "I~a" x))
    ((and (rationalp x) (not (integerp x)))
     (format nil "Q~a/~a" (numerator x) (denominator x)))
    ((floatp x)
     (multiple-value-bind (sig exp sign) (integer-decode-float x)
       (format nil "~a~a/~a/~a" (if (typep x 'double-float) "D" "F")
               sig exp sign)))
    ((characterp x) (format nil "C~a" (char-code x)))
    ((stringp x) (format nil "S(~a)" (codes-string x)))
    ((symbolp x)
     (let ((p (symbol-package x)))
       (format nil "Y~a:~a"
               (if p (codes-string (package-name p)) "NIL")
               (codes-string (symbol-name x)))))
    ((consp x) (format nil "(~a.~a)" (enc (car x)) (enc (cdr x))))
    ((vectorp x)
     (format nil "V~a~a" (enc (type-of x))
             (let ((first t))
               (with-output-to-string (o)
                 (write-char #\[ o)
                 (loop for e across x do
                   (if first (setf first nil) (write-char #\, o))
                   (write-string (enc e) o))
                 (write-char #\] o)))))
    ((arrayp x)
     (format nil "A~a~a~a" (enc (array-dimensions x)) (enc (array-element-type x))
             (let ((first t))
               (with-output-to-string (o)
                 (write-char #\[ o)
                 (loop for i below (array-total-size x) do
                   (if first (setf first nil) (write-char #\, o))
                   (write-string (enc (row-major-aref x i)) o))
                 (write-char #\] o)))))
    (t (format nil "?~a" (enc (type-of x))))))

(defun probe (thunk)
  (handler-case
      (list :ok (multiple-value-list (funcall thunk)))
    (error (c) (list :err c))))

(defun result-field (r)
  (if (eq (first r) :ok)
      (format nil "~{~a~^;~}" (mapcar #'enc (second r)))
      (format nil "ERR.~a" (enc (class-name (class-of (second r)))))))

(defun okp (r) (eq (first r) :ok))

(defun first-value (r) (first (second r)))

(defun fields (&rest fs)
  (format nil "~{~a:~a~^ ~}" fs))

(defun code-list (codes) (map 'list #'code-char codes))

(defun build-string (spec)
  (let ((kind (first spec)))
    (ecase kind
      (:simple
       (let ((codes (second spec)))
         (let ((s (make-string (length codes))))
           (loop for c in codes for i from 0 do (setf (char s i) (code-char c)))
           s)))
      (:base
       (let ((codes (second spec)))
         (let ((s (make-array (length codes) :element-type 'base-char
                                             :initial-element (code-char 32))))
           (loop for c in codes for i from 0 do (setf (char s i) (code-char c)))
           s)))
      (:fill
       (let* ((codes (second spec))
              (extra (third spec))
              (s (make-array (+ (length codes) extra)
                             :element-type 'character
                             :fill-pointer (length codes)
                             :initial-element #\Z)))
         (loop for c in codes for i from 0 do (setf (char s i) (code-char c)))
         s))
      (:adjustable
       (let ((codes (second spec)))
         (make-array (length codes) :element-type 'character :adjustable t
                                   :initial-contents (code-list codes))))
      (:displaced
       (let* ((prefix (second spec))
              (codes (third spec))
              (suffix (fourth spec))
              (all (append prefix codes suffix))
              (buffer (make-array (length all) :element-type 'character
                                              :initial-contents (code-list all))))
         (make-array (length codes) :element-type 'character
                                   :displaced-to buffer
                                   :displaced-index-offset (length prefix)))))))

(defun symbol-for (index)
  (case index
    (0 'car)
    (1 :foo)
    (2 (find-symbol "HELLO" "PROBE-PKG"))
    (3 (find-symbol "hello world" "PROBE-PKG"))
    (4 (make-symbol "fixed-gensym"))
    (5 (find-symbol "MiXeD" "PROBE-PKG"))
    (6 (find-symbol "A(B" "PROBE-PKG"))
    (7 (find-symbol "123" "PROBE-PKG"))
    (8 (intern "BAR" "KEYWORD"))
    (t (find-symbol "CL-USER-SYMBOL" "PROBE-PKG"))))

(defun build-object (spec)
  (let ((kind (first spec)))
    (ecase kind
      (:integer (second spec))
      (:ratio (/ (second spec) (third spec)))
      (:double (second spec))
      (:single (second spec))
      (:char (code-char (second spec)))
      (:string (build-string (second spec)))
      (:symbol (symbol-for (second spec)))
      (:list (mapcar #'build-object (second spec)))
      (:vector (coerce (mapcar #'build-object (second spec)) 'vector))
      (:array2
       (let ((rows (second spec)) (cols (third spec)) (elems (fourth spec)))
         (make-array (list rows cols)
                     :initial-contents
                     (loop for r below rows
                           collect (loop for c below cols
                                         collect (build-object
                                                  (nth (mod (+ (* r cols) c)
                                                            (length elems))
                                                       elems)))))))
      (:circular
       (let* ((elems (mapcar #'build-object (second spec)))
              (head (copy-list elems)))
         (setf (cdr (last head)) head)
         head))
      (:struct (make-probe-struct :a (second spec)
                                  :b (build-object (third spec)))))))

(defstruct probe-struct a b)

(defun case-fn (op)
  (case op
    (string-upcase #'string-upcase)
    (string-downcase #'string-downcase)
    (string-capitalize #'string-capitalize)
    (nstring-upcase #'nstring-upcase)
    (nstring-downcase #'nstring-downcase)
    (nstring-capitalize #'nstring-capitalize)))

(defun nstring-op-p (op)
  (member op '(nstring-upcase nstring-downcase nstring-capitalize)))

(defun run-case (op spec start end)
  (let* ((s (build-string spec))
         (r (probe (lambda () (funcall (case-fn op) s :start start :end end)))))
    (if (nstring-op-p op)
        (fields :R (result-field r)
              :EQ (if (and (okp r) (eq (first-value r) s)) "T" "NIL")
              :ARG (enc s))
        (fields :R (result-field r)))))

(defun bag-for (kind codes)
  (ecase kind
    (:list (code-list codes))
    (:string (let ((s (make-string (length codes))))
               (loop for c in codes for i from 0 do (setf (char s i) (code-char c)))
               s))
    (:vector (coerce (code-list codes) 'vector))))

(defun trim-fn (op)
  (case op
    (string-trim #'string-trim)
    (string-left-trim #'string-left-trim)
    (string-right-trim #'string-right-trim)))

(defun run-trim (op bag-kind bag-codes spec)
  (let* ((bag (bag-for bag-kind bag-codes))
         (s (build-string spec))
         (r (probe (lambda () (funcall (trim-fn op) bag s)))))
    (fields :R (result-field r)
          :EQ (if (and (okp r) (eq (first-value r) s)) "T" "NIL")
          :TY (if (okp r) (enc (type-of (first-value r))) "NIL"))))

(defun cmp-fn (op)
  (case op
    (string= #'string=)
    (string/= #'string/=)
    (string< #'string<)
    (string> #'string>)
    (string<= #'string<=)
    (string>= #'string>=)
    (string-equal #'string-equal)
    (string-not-equal #'string-not-equal)
    (string-lessp #'string-lessp)
    (string-greaterp #'string-greaterp)
    (string-not-greaterp #'string-not-greaterp)
    (string-not-lessp #'string-not-lessp)))

(defun run-cmp (op spec1 spec2 s1 e1 s2 e2)
  (let ((r (probe (lambda ()
                    (funcall (cmp-fn op) (build-string spec1) (build-string spec2)
                             :start1 s1 :end1 e1 :start2 s2 :end2 e2)))))
    (fields :R (result-field r))))

(defun run-search (spec1 spec2 s1 e1 s2 e2)
  (let ((r (probe (lambda ()
                    (search (build-string spec1) (build-string spec2)
                            :start1 s1 :end1 e1 :start2 s2 :end2 e2)))))
    (fields :R (result-field r))))

(defun run-position (item spec start end)
  (let ((r (probe (lambda ()
                    (position (code-char item) (build-string spec)
                              :start start :end end)))))
    (fields :R (result-field r))))

(defun run-subseq (spec start end)
  (let ((r (probe (lambda () (subseq (build-string spec) start end)))))
    (fields :R (result-field r))))

(defun run-replace (spec1 spec2 s1 e1 s2 e2)
  (let* ((dst (build-string spec1))
         (src (build-string spec2))
         (r (probe (lambda () (replace dst src :start1 s1 :end1 e1
                                             :start2 s2 :end2 e2)))))
    (fields :R (result-field r)
          :EQ (if (and (okp r) (eq (first-value r) dst)) "T" "NIL")
          :DST (enc dst))))

(defun run-parse-integer (spec start end radix junk)
  (let ((r (probe (lambda ()
                    (parse-integer (build-string spec) :start start :end end
                                   :radix radix :junk-allowed junk)))))
    (fields :R (result-field r))))

(defun run-string (spec)
  (let ((r (probe (lambda () (string (build-object spec))))))
    (fields :R (result-field r)
          :TY (if (okp r) (enc (type-of (first-value r))) "NIL"))))

(defun run-coerce (spec)
  (let ((r (probe (lambda () (coerce (build-string spec) 'simple-string)))))
    (fields :R (result-field r)
          :TY (if (okp r) (enc (type-of (first-value r))) "NIL"))))

(defun run-concatenate (spec1 spec2)
  (let ((r (probe (lambda ()
                    (concatenate 'string (build-string spec1) (build-string spec2))))))
    (fields :R (result-field r)
          :TY (if (okp r) (enc (type-of (first-value r))) "NIL"))))

(defun run-wots (spec text-codes)
  (let* ((dst (build-string spec))
         (text (build-string (list :simple text-codes)))
         (r (probe (lambda ()
                     (with-output-to-string (o dst)
                       (write-string text o))))))
    (fields :R (result-field r)
          :EQ (if (and (okp r) (eq (first-value r) dst)) "T" "NIL")
          :TY (if (okp r) (enc (type-of (first-value r))) "NIL")
          :DST (enc dst))))

(defun print-fn (op)
  (case op
    (write-to-string #'write-to-string)
    (prin1-to-string #'prin1-to-string)
    (princ-to-string #'princ-to-string)))

(defun print-var-symbol (keyword)
  (case keyword
    (:base '*print-base*)
    (:radix '*print-radix*)
    (:case '*print-case*)
    (:escape '*print-escape*)
    (:readably '*print-readably*)
    (:length '*print-length*)
    (:level '*print-level*)
    (:array '*print-array*)
    (:gensym '*print-gensym*)
    (:circle '*print-circle*)
    (:pretty '*print-pretty*)))

(defun run-print (op object-spec keys)
  (let ((r (probe (lambda ()
                     (let ((*print-pretty* nil)
                           (flat (apply #'append keys)))
                       (if (eq op 'write-to-string)
                           (apply #'write-to-string (build-object object-spec) flat)
                           (progv (mapcar (lambda (pair) (print-var-symbol (first pair))) keys)
                                  (mapcar #'second keys)
                                  (funcall (print-fn op) (build-object object-spec)))))))))
    (fields :R (result-field r))))

(in-package :cl-user)

(deftest nss-diff.1
  (nss-test::run-case 'STRING-UPCASE '(:BASE (9)) 0 0)
  "R:S(9)")

(deftest nss-diff.2
  (nss-test::run-case 'STRING-UPCASE '(:ADJUSTABLE (50 99)) 0 1)
  "R:S(50,99)")

(deftest nss-diff.3
  (nss-test::run-case 'STRING-UPCASE '(:DISPLACED NIL (233 66 1040) (98 65)) 0 2)
  "R:S(201,66,1040)")

(deftest nss-diff.41
  (nss-test::run-case 'STRING-DOWNCASE '(:ADJUSTABLE (49 45 65 201 1040 98)) 0 0)
  "R:S(49,45,65,201,1040,98)")

(deftest nss-diff.42
  (nss-test::run-case 'STRING-DOWNCASE '(:FILL (1040 233 50 97 45 1040 98 46) 2) 0 1)
  "R:S(1072,233,50,97,45,1040,98,46)")

(deftest nss-diff.43
  (nss-test::run-case 'STRING-DOWNCASE '(:FILL (9 45 98 65 98) 1) 0 2)
  "R:S(9,45,98,65,98)")

(deftest nss-diff.81
  (nss-test::run-case 'STRING-CAPITALIZE '(:FILL (97 9 50 9) 2) 0 0)
  "R:S(97,9,50,9)")

(deftest nss-diff.82
  (nss-test::run-case 'STRING-CAPITALIZE '(:FILL (99 65 45 49 65 48 9 99 45 945 945 99) 2) 0 1)
  "R:S(67,65,45,49,65,48,9,99,45,945,945,99)")

(deftest nss-diff.83
  (nss-test::run-case 'STRING-CAPITALIZE '(:ADJUSTABLE (50 32 32 9 65 65 45 49 945 45 48 66 45 50 97 48 98 65 45 45)) 0 10)
  "R:S(50,32,32,9,65,97,45,49,945,45,48,66,45,50,97,48,98,65,45,45)")

(deftest nss-diff.121
  (nss-test::run-case 'NSTRING-UPCASE '(:BASE (65 97 45 48 97 50 9 97)) 0 0)
  "R:S(65,97,45,48,97,50,9,97) EQ:T ARG:S(65,97,45,48,97,50,9,97)")

(deftest nss-diff.122
  (nss-test::run-case 'NSTRING-UPCASE '(:SIMPLE (67 50 66 201 66 233 97 945 97 48 48 50 233 66 97 99 99 1040 65 97)) 0 1)
  "R:S(67,50,66,201,66,233,97,945,97,48,48,50,233,66,97,99,99,1040,65,97) EQ:T ARG:S(67,50,66,201,66,233,97,945,97,48,48,50,233,66,97,99,99,1040,65,97)")

(deftest nss-diff.124
  (nss-test::run-case 'NSTRING-UPCASE '(:SIMPLE (49 98 201 9 66 65)) 0 5)
  "R:S(49,66,201,9,66,65) EQ:T ARG:S(49,66,201,9,66,65)")

(deftest nss-diff.161
  (nss-test::run-case 'NSTRING-DOWNCASE '(:ADJUSTABLE (1040 65 46)) 0 0)
  "R:S(1040,65,46) EQ:T ARG:S(1040,65,46)")

(deftest nss-diff.162
  (nss-test::run-case 'NSTRING-DOWNCASE '(:DISPLACED (9) (48 45 1040 45 66 945) (1040 233)) 0 1)
  "R:S(48,45,1040,45,66,945) EQ:T ARG:S(48,45,1040,45,66,945)")

(deftest nss-diff.163
  (nss-test::run-case 'NSTRING-DOWNCASE '(:ADJUSTABLE (46 50 32 50 65 45 66)) 0 3)
  "R:S(46,50,32,50,65,45,66) EQ:T ARG:S(46,50,32,50,65,45,66)")

(deftest nss-diff.201
  (nss-test::run-case 'NSTRING-CAPITALIZE '(:ADJUSTABLE (32)) 0 0)
  "R:S(32) EQ:T ARG:S(32)")

(deftest nss-diff.202
  (nss-test::run-case 'NSTRING-CAPITALIZE '(:BASE (50 48 46 67 98 48 50)) 0 1)
  "R:S(50,48,46,67,98,48,50) EQ:T ARG:S(50,48,46,67,98,48,50)")

(deftest nss-diff.203
  (nss-test::run-case 'NSTRING-CAPITALIZE '(:DISPLACED NIL (98 66 48 98 97 1040 48 50 97 67 50 46 49 66 46 201 945 45 49 1040) NIL) 0 10)
  "R:S(66,98,48,98,97,1072,48,50,97,99,50,46,49,66,46,201,945,45,49,1040) EQ:T ARG:S(66,98,48,98,97,1072,48,50,97,99,50,46,49,66,46,201,945,45,49,1040)")

(deftest nss-diff.241
  (nss-test::run-trim 'STRING-TRIM ':VECTOR '(945 201 201) '(:DISPLACED (98 945) (1040) (99 1040)))
  "R:S(1040) EQ:T TY:(Y67,79,77,77,79,78,45,76,73,83,80:86,69,67,84,79,82.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.(I1.NIL)))")

(deftest nss-diff.251
  (nss-test::run-trim 'STRING-LEFT-TRIM ':STRING 'NIL '(:SIMPLE NIL))
  "R:S() EQ:T TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I0.NIL).NIL)))")

(deftest nss-diff.261
  (nss-test::run-trim 'STRING-RIGHT-TRIM ':VECTOR '(201 1040) '(:DISPLACED NIL (1040 46 46 233 49 67) (66 67)))
  "R:S(1040,46,46,233,49,67) EQ:T TY:(Y67,79,77,77,79,78,45,76,73,83,80:86,69,67,84,79,82.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.(I6.NIL)))")

(deftest nss-diff.271
  (nss-test::run-cmp 'STRING= '(:DISPLACED (99 945) (67) NIL) '(:FILL (97 233 32 99 9) 2) 0 0 0 0)
  "R:T")

(deftest nss-diff.272
  (nss-test::run-cmp 'STRING= '(:SIMPLE (97 67 1040 201 233)) '(:SIMPLE NIL) 0 1 0 NIL)
  "R:NIL")

(deftest nss-diff.311
  (nss-test::run-cmp 'STRING/= '(:ADJUSTABLE (48 945 32)) '(:FILL (67 99 9 45 233) 2) 0 0 0 0)
  "R:NIL")

(deftest nss-diff.312
  (nss-test::run-cmp 'STRING/= '(:FILL (1040 98 48) 2) '(:SIMPLE (46 45)) 0 1 1 1)
  "R:I0")

(deftest nss-diff.351
  (nss-test::run-cmp 'STRING< '(:ADJUSTABLE (9 50 49 1040)) '(:DISPLACED (66) (945 97 67 99 45 67 201 65 32 65 48 45) (1040)) 0 0 0 0)
  "R:NIL")

(deftest nss-diff.352
  (nss-test::run-cmp 'STRING< '(:BASE (97 48 66 9)) '(:SIMPLE (66 9 99 50)) 0 1 0 NIL)
  "R:NIL")

(deftest nss-diff.391
  (nss-test::run-cmp 'STRING> '(:SIMPLE (98 9 201 97 48 945 9)) '(:SIMPLE (67)) 0 0 0 0)
  "R:NIL")

(deftest nss-diff.392
  (nss-test::run-cmp 'STRING> '(:DISPLACED (945 97) (49 67) (46 66)) '(:DISPLACED NIL (48 32 98 1040 1040 201 66) NIL) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.431
  (nss-test::run-cmp 'STRING<= '(:FILL (67 49 233 48) 3) '(:ADJUSTABLE (32)) 0 0 0 0)
  "R:I0")

(deftest nss-diff.432
  (nss-test::run-cmp 'STRING<= '(:SIMPLE (46 9 233 50 46)) '(:BASE (67 45 98 48)) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.471
  (nss-test::run-cmp 'STRING>= '(:DISPLACED NIL (98) (48 67)) '(:DISPLACED (66) (46 1040 66 9) (50)) 0 0 0 0)
  "R:I0")

(deftest nss-diff.472
  (nss-test::run-cmp 'STRING>= '(:ADJUSTABLE (98 65 48)) '(:BASE (97 48 49 66 48 32)) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.511
  (nss-test::run-cmp 'STRING-EQUAL '(:DISPLACED (48) (66 66 66 99 1040 233 48 48 65 99 9 32) (1040)) '(:DISPLACED NIL (201) NIL) 0 0 0 0)
  "R:T")

(deftest nss-diff.512
  (nss-test::run-cmp 'STRING-EQUAL '(:BASE (66 50 98 49 32 9 50 50)) '(:DISPLACED (1040 66) (65 45 45 49) (48)) 0 1 0 NIL)
  "R:NIL")

(deftest nss-diff.551
  (nss-test::run-cmp 'STRING-NOT-EQUAL '(:ADJUSTABLE (50 9 201)) '(:DISPLACED (1040 99) (48 65 65 48 48 201 32 32 46 97 46 32) NIL) 0 0 0 0)
  "R:NIL")

(deftest nss-diff.552
  (nss-test::run-cmp 'STRING-NOT-EQUAL '(:ADJUSTABLE (945 97 945 32 49)) '(:FILL NIL 3) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.591
  (nss-test::run-cmp 'STRING-LESSP '(:ADJUSTABLE (48)) '(:BASE (48 67 98 97 9 98)) 0 0 0 0)
  "R:NIL")

(deftest nss-diff.592
  (nss-test::run-cmp 'STRING-LESSP '(:DISPLACED (48 49) (99 99 233 1040 32 98 48 201 97 9 201 49 45 50 65 9 97 97 66 945) (32)) '(:FILL (1040) 3) 0 1 1 NIL)
  "R:NIL")

(deftest nss-diff.632
  (nss-test::run-cmp 'STRING-GREATERP '(:FILL (46 97 46 945 49 67) 2) '(:BASE (50)) 0 1 1 NIL)
  "R:I0")

(deftest nss-diff.634
  (nss-test::run-cmp 'STRING-GREATERP '(:SIMPLE (67 201 32 67)) '(:ADJUSTABLE (50 99 32 98 67 945 98 97 945 99 32 67 66 65 98 1040 65 98 67 1040)) 0 3 10 19)
  "R:I0")

(deftest nss-diff.671
  (nss-test::run-cmp 'STRING-NOT-GREATERP '(:BASE (98 65 45 49 48 98 32 9 49 48 48 97)) '(:ADJUSTABLE (1040 9 65)) 0 0 0 0)
  "R:I0")

(deftest nss-diff.672
  (nss-test::run-cmp 'STRING-NOT-GREATERP '(:FILL (49 65 201 32) 3) '(:ADJUSTABLE (99 50 46 97 97)) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.711
  (nss-test::run-cmp 'STRING-NOT-LESSP '(:FILL (48) 2) '(:DISPLACED (66) (9) (201 65)) 0 0 0 0)
  "R:I0")

(deftest nss-diff.712
  (nss-test::run-cmp 'STRING-NOT-LESSP '(:DISPLACED NIL (201 9 46 201 99 46) (1040 201)) '(:FILL (46 50 945 201) 2) 0 1 0 NIL)
  "R:I0")

(deftest nss-diff.751
  (nss-test::run-search '(:BASE (32)) '(:FILL (50 98 98 1040 99 99) 1) 0 0 0 0)
  "R:I0")

(deftest nss-diff.752
  (nss-test::run-search '(:SIMPLE (99 65 49 233 48 97 67 49 50 45 48 201)) '(:ADJUSTABLE (46 49 67 98 46 66 50)) 0 1 0 NIL)
  "R:NIL")

(deftest nss-diff.754
  (nss-test::run-search '(:BASE (48 45)) '(:DISPLACED NIL (32 46 9 233 49 201 97) (50)) 0 NIL 3 6)
  "R:NIL")

(deftest nss-diff.791
  (nss-test::run-position 98 '(:SIMPLE (46 66 49 99 49 45)) 0 0)
  "R:NIL")

(deftest nss-diff.792
  (nss-test::run-position 99 '(:DISPLACED (9) (97 45 66 98) (65)) 0 1)
  "R:NIL")

(deftest nss-diff.793
  (nss-test::run-position 46 '(:ADJUSTABLE (9 99 945 1040 49 32)) 0 3)
  "R:NIL")

(deftest nss-diff.831
  (nss-test::run-subseq '(:ADJUSTABLE (233 50 99 45 97)) 0 0)
  "R:S()")

(deftest nss-diff.833
  (nss-test::run-subseq '(:SIMPLE (46 945 65 201 233)) 0 2)
  "R:S(46,945)")

(deftest nss-diff.834
  (nss-test::run-subseq '(:FILL (1040 97 945 32) 2) 0 3)
  "R:S(1040,97,945)")

(deftest nss-diff.871
  (nss-test::run-replace '(:BASE (48 32 46 97 98 45 46 49 98 97 98 65)) '(:FILL (201 65 201 97 233) 1) 0 0 0 0)
  "R:S(48,32,46,97,98,45,46,49,98,97,98,65) EQ:T DST:S(48,32,46,97,98,45,46,49,98,97,98,65)")

(deftest nss-diff.872
  (nss-test::run-replace '(:SIMPLE (99 50)) '(:DISPLACED NIL (66 9 98 46 99 45 48) NIL) 0 1 0 NIL)
  "R:S(66,50) EQ:T DST:S(66,50)")

(deftest nss-diff.873
  (nss-test::run-replace '(:BASE (65 45 99)) '(:DISPLACED NIL (97 49 99 67 945 1040 97) (49)) 0 2 1 7)
  "R:S(49,99,99) EQ:T DST:S(49,99,99)")

(deftest nss-diff.912
  (nss-test::run-parse-integer '(:ADJUSTABLE (67 98)) 0 1 36 NIL)
  "R:I12;I1")

(deftest nss-diff.915
  (nss-test::run-parse-integer '(:SIMPLE (65 65 66 98 233 49 67 49)) 0 8 16 T)
  "R:I43707;I4")

(deftest nss-diff.916
  (nss-test::run-parse-integer '(:BASE (46 48 46 66 46 32)) 0 NIL 8 T)
  "R:NIL;I0")

(deftest nss-diff.951
  (nss-test::run-string '(:SYMBOL 9))
  "R:S(67,76,45,85,83,69,82,45,83,89,77,66,79,76) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I14.NIL).NIL)))")

(deftest nss-diff.952
  (nss-test::run-string '(:SYMBOL 9))
  "R:S(67,76,45,85,83,69,82,45,83,89,77,66,79,76) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I14.NIL).NIL)))")

(deftest nss-diff.971
  (nss-test::run-coerce '(:DISPLACED (98) (233 49 46 45 65 98 97 233 49 233 46 98) (98)))
  "R:S(233,49,46,45,65,98,97,233,49,233,46,98) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I12.NIL).NIL)))")

(deftest nss-diff.972
  (nss-test::run-coerce '(:SIMPLE (45 49 233 9)))
  "R:S(45,49,233,9) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I4.NIL).NIL)))")

(deftest nss-diff.991
  (nss-test::run-concatenate '(:SIMPLE (49 1040 201)) '(:FILL (1040 67 45) 2))
  "R:S(49,1040,201,1040,67,45) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I6.NIL).NIL)))")

(deftest nss-diff.992
  (nss-test::run-concatenate '(:SIMPLE (99 48 201 97 50 65 98)) '(:DISPLACED NIL (46 67 201 201 45 67 32 46 45 65 1040 46 67 48 45 32 46 50 67 233) (201 50)))
  "R:S(99,48,201,97,50,65,98,46,67,201,201,45,67,32,46,45,65,1040,46,67,48,45,32,46,50,67,233) TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I27.NIL).NIL)))")

(deftest nss-diff.1011
  (nss-test::run-wots '(:FILL (99 97 48 66) 1) '(233 201 66))
  "R:S(233,201,66) EQ:NIL TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I3.NIL).NIL))) DST:S(99,97,48,66,233,201,66)")

(deftest nss-diff.1012
  (nss-test::run-wots '(:FILL (66 99) 2) '(9 48 233))
  "R:S(9,48,233) EQ:NIL TY:(Y67,79,77,77,79,78,45,76,73,83,80:83,73,77,80,76,69,45,65,82,82,65,89.(Y67,79,77,77,79,78,45,76,73,83,80:67,72,65,82,65,67,84,69,82.((I3.NIL).NIL))) DST:S(66,99,9,48,233)")

(deftest nss-diff.1031
  (nss-test::run-print 'PRIN1-TO-STRING '(:CIRCULAR ((:INTEGER -262) (:DOUBLE 123.456d0) (:DOUBLE 1.0d-10) (:RATIO 4 8) (:DOUBLE 3.141592653589793d0))) '((:BASE 10) (:CIRCLE T)))
  "R:S(35,49,61,40,45,50,54,50,32,49,50,51,46,52,53,54,100,48,32,49,46,48,100,45,49,48,32,49,47,50,32,51,46,49,52,49,53,57,50,54,53,51,53,56,57,55,57,51,100,48,32,46,32,35,49,35,41)")

(deftest nss-diff.1033
  (nss-test::run-print 'WRITE-TO-STRING '(:INTEGER 35) '((:LENGTH NIL)))
  "R:S(51,53)")

(deftest nss-diff.1034
  (nss-test::run-print 'PRIN1-TO-STRING '(:RATIO 0 1) '((:LEVEL 0)))
  "R:S(48)")

(deftest nss-diff.1035
  (nss-test::run-print 'PRINC-TO-STRING '(:DOUBLE -0.001d0) '((:LEVEL 1)))
  "R:S(45,48,46,48,48,49,100,48)")

(deftest nss-diff.1036
  (nss-test::run-print 'WRITE-TO-STRING '(:SINGLE -2.5) '((:LEVEL 2)))
  "R:S(45,50,46,53)")

(deftest nss-diff.1037
  (nss-test::run-print 'PRIN1-TO-STRING '(:CHAR 49) '((:LEVEL NIL) (:ARRAY T)))
  "R:S(35,92,49)")

(deftest nss-diff.1038
  (nss-test::run-print 'PRINC-TO-STRING '(:STRING (:DISPLACED (66 945) (48 32 97 1040 99 201 32 32 66 99 50 97) (201))) '((:ARRAY T)))
  "R:S(48,32,97,1040,99,201,32,32,66,99,50,97)")

(deftest nss-diff.1040
  (nss-test::run-print 'PRIN1-TO-STRING '(:LIST ((:LIST ((:DOUBLE 1.0d10) (:DOUBLE 1.0d10) (:RATIO -3 9))) (:LIST ((:RATIO 4 2) (:RATIO -8 6) (:INTEGER 729))))) '((:GENSYM T)))
  "R:S(40,40,49,46,48,100,49,48,32,49,46,48,100,49,48,32,45,49,47,51,41,32,40,50,32,45,52,47,51,32,55,50,57,41,41)")

(deftest nss-diff.1045
  (nss-test::run-print 'WRITE-TO-STRING '(:INTEGER 0) '((:BASE 2) (:READABLY NIL)))
  "R:S(48)")

(deftest nss-diff.1050
  (nss-test::run-print 'PRINC-TO-STRING '(:STRING (:ADJUSTABLE (233 32 1040 945 9 45 1040))) '((:RADIX T) (:CASE :CAPITALIZE)))
  "R:S(233,32,1040,945,9,45,1040)")

(deftest nss-diff.1073
  (nss-test::run-print 'PRIN1-TO-STRING '(:CHAR 49) '((:PRETTY NIL) (:ESCAPE T)))
  "R:S(35,92,49)")

;;; Non-integer :END through the SEARCH, MISMATCH and REPLACE keyword paths.

(deftest nss-diff.search-end2-non-integer
  (handler-case (progn (search "ab" "xxab" :end2 :bad) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.search-end1-non-integer
  (handler-case (progn (search "ab" "xxab" :end1 :bad) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.search-three-keyword-pairs-end1-non-integer
  (handler-case
      (progn (search "ab" "xxab" :from-end nil :test #'char= :end1 :bad)
             :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.search-generic-entry-end1-non-integer
  (handler-case
      (progn (apply #'search
                    (list "ab" "xxab" :from-end nil :test #'char= :key #'identity
                          :end1 :bad))
             :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.search-end-nil-is-default
  (list (search "ab" "xxab" :end1 nil)
        (search "ab" "xxab" :end2 nil))
  (2 2))

(deftest nss-diff.mismatch-end1-non-integer
  (handler-case (progn (mismatch "ab" "ab" :end1 :bad) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.mismatch-end2-non-integer-generic
  (handler-case
      (progn (mismatch "ab" "ab" :end2 :bad :from-end nil :test #'char=)
             :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.mismatch-end-nil-is-default
  (mismatch "ab" "ab" :end1 nil)
  nil)

(deftest nss-diff.replace-end1-non-integer
  (handler-case
      (progn (replace (make-string 2 :initial-element #\x) "ab" :end1 :bad)
             :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.replace-end2-non-integer-generic
  (handler-case
      (progn (replace (make-string 2 :initial-element #\x) "ab"
                      :end2 :bad :allow-other-keys t)
             :no-error)
    (type-error () :type-error))
  :type-error)

(deftest nss-diff.replace-end-nil-is-default
  (let ((dst (make-string 2 :initial-element #\x)))
    (replace dst "ab" :end1 nil :end2 nil)
    dst)
  "ab")
