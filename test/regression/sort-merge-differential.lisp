;;; SORT / STABLE-SORT / MERGE regression probes; expected lines are SBCL's.


(defun %sm-enc (x)
  (cond
    ((null x) "NIL")
    ((eq x t) "T")
    ((integerp x) (format nil "I~a" x))
    ((and (rationalp x) (not (integerp x)))
     (format nil "Q~a/~a" (numerator x) (denominator x)))
    ((typep x 'double-float)
     (multiple-value-bind (sig exp sign) (integer-decode-float x)
       (if (zerop sig)
           (format nil "F0/~a" sign)
           (format nil "F~a/~a/~a" sig exp sign))))
    ((characterp x) (format nil "C~a" (char-code x)))
    ((stringp x)
     (with-output-to-string (s)
       (write-char #\S s)
       (write-char #\( s)
       (let ((first t))
         (loop for c across x
               do (if first (setf first nil) (write-char #\, s))
                  (write-string (format nil "~a" (char-code c)) s)))
       (write-char #\) s)))
    ((consp x) (format nil "(~a.~a)" (%sm-enc (car x)) (%sm-enc (cdr x))))
    ((symbolp x) (format nil "Y~a" (symbol-name x)))
    (t (format nil "?~a" (type-of x)))))

(defun %sm-enc-seq (l)
  (with-output-to-string (s)
    (write-char #\[ s)
    (let ((first t))
      (dolist (e l)
        (if first (setf first nil) (write-char #\, s))
        (write-string (%sm-enc e) s)))
    (write-char #\] s)))

(defun %sm-enc-type (x) (%sm-enc (type-of x)))

(defun %sm-same-multiset-p (a b)
  (let ((ha (make-hash-table :test #'equal))
        (hb (make-hash-table :test #'equal)))
    (dolist (e a) (let ((k (%sm-enc e))) (setf (gethash k ha) (1+ (gethash k ha 0)))))
    (dolist (e b) (let ((k (%sm-enc e))) (setf (gethash k hb) (1+ (gethash k hb 0)))))
    (and (= (hash-table-count ha) (hash-table-count hb))
         (loop for k being the hash-keys of ha using (hash-value v)
               always (= v (gethash k hb 0))))))

(defun %sm-stable-p (l)
  (let ((prev-key nil) (prev-index nil) (first t) (ok t))
    (dolist (e l ok)
      (when (consp e)
        (if first
            (setf first nil)
            (when (equal (car e) prev-key)
              (unless (<= prev-index (cdr e)) (setf ok nil))))
        (setf prev-key (car e) prev-index (cdr e))))))

(defun %sm-total-less (a b)
  (or (< (mod a 13) (mod b 13))
      (and (= (mod a 13) (mod b 13)) (< a b))))

(defun %sm-pred-base (name)
  (cond ((eq name 'lt) #'<)
        ((eq name 'gt) #'>)
        ((eq name 'le) #'<=)
        ((eq name 'ge) #'>=)
        ((eq name 'stringlt) #'string<)
        ((eq name 'charlt) #'char<)
        ((eq name 'true) (lambda (a b) (declare (ignore a b)) t))
        ((eq name 'false) (lambda (a b) (declare (ignore a b)) nil))
        ((eq name 'total) #'%sm-total-less)
        ((eq name 'bad) (lambda (a b) (or (< a b) (evenp a))))
        (t (error "unknown predicate ~a" name))))

(defun %sm-key-base (name)
  (cond ((eq name 'identity) #'identity)
        ((eq name 'count) #'identity)
        ((eq name 'car) #'car)
        ((eq name 'neg) #'-)
        (t (error "unknown key ~a" name))))

(defun %sm-make-pred (name counter)
  (let ((base (%sm-pred-base name)))
    (lambda (a b)
      (setf (car counter) (1+ (car counter)))
      (funcall base a b))))

(defun %sm-make-key (name counter)
  (let ((base (%sm-key-base name)))
    (lambda (x)
      (setf (car counter) (1+ (car counter)))
      (funcall base x))))

(defun %sm-sorted-p (l pred key)
  (if (member pred '(le ge true false bad))
      "NA"
      (let ((base (%sm-pred-base pred))
            (kb (and key (not (eq key 'none)) (%sm-key-base key)))
            (ok t) (prev nil) (first t))
        (dolist (e l)
          (let ((k (if kb (funcall kb e) e)))
            (when (and (not first) (funcall base k prev)) (setf ok nil))
            (setf prev k first nil)))
        (if ok "T" "NIL"))))

(defun %sm-stable-field (op l)
  (if (member op '(stable-sort merge))
      (if (%sm-stable-p l) "T" "NIL")
      "NA"))

(defun %sm-build-container (container data)
  (let ((n (length data)))
    (cond
      ((eq container 'list) (values (copy-list data) nil))
      ((eq container 'svector)
       (values (make-array n :initial-contents data) nil))
      ((eq container 'u8)
       (values (make-array n :element-type '(unsigned-byte 8)
                             :initial-contents data) nil))
      ((eq container 'df)
       (values (make-array n :element-type 'double-float
                             :initial-contents data) nil))
      ((eq container 'sstring)
       (values (make-array n :element-type 'character
                             :initial-contents data) nil))
      ((eq container 'bstring)
       (values (make-array n :element-type 'base-char
                             :initial-contents data) nil))
      ((eq container 'bits)
       (values (make-array n :element-type 'bit
                             :initial-contents data) nil))
      ((eq container 'fill)
       (let ((v (make-array (+ n 3) :adjustable t :fill-pointer n
                                  :initial-element 777)))
         (loop for e in data for i from 0 do (setf (aref v i) e))
         (values v nil)))
      ((eq container 'fill-u8)
       (let ((v (make-array (+ n 3) :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer n
                                  :initial-element 255)))
         (loop for e in data for i from 0 do (setf (aref v i) e))
         (values v nil)))
      ((eq container 'fill-df)
       (let ((v (make-array (+ n 3) :element-type 'double-float
                                  :adjustable t :fill-pointer n
                                  :initial-element 7.0d0)))
         (loop for e in data for i from 0 do (setf (aref v i) e))
         (values v nil)))
      ((eq container 'fill-bits)
       (let ((v (make-array (+ n 3) :element-type 'bit
                                  :adjustable t :fill-pointer n
                                  :initial-element 1)))
         (loop for e in data for i from 0 do (setf (aref v i) e))
         (values v nil)))
      ((eq container 'fill-char)
       (let ((v (make-array (+ n 3) :element-type 'character
                                  :adjustable t :fill-pointer n
                                  :initial-element #\z)))
         (loop for e in data for i from 0 do (setf (aref v i) e))
         (values v nil)))
      ((eq container 'displaced)
       (let* ((off 3)
              (base (make-array (+ n off 3) :initial-element 555)))
         (loop for e in data for i from 0 do (setf (aref base (+ off i)) e))
         (values (make-array n :displaced-to base
                               :displaced-index-offset off)
                 base)))
      ((eq container 'displaced-u8)
       (let* ((off 3)
              (base (make-array (+ n off 3) :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
         (loop for e in data for i from 0 do (setf (aref base (+ off i)) e))
         (values (make-array n :displaced-to base
                               :displaced-index-offset off
                               :element-type '(unsigned-byte 8))
                 base)))
      ((eq container 'displaced-bits)
       (let* ((off 3)
              (base (make-array (+ n off 3) :element-type 'bit
                                           :initial-element 0)))
         (loop for e in data for i from 0 do (setf (aref base (+ off i)) e))
         (values (make-array n :displaced-to base
                               :displaced-index-offset off
                               :element-type 'bit)
                 base)))
      ((eq container 'displaced-char)
       (let* ((off 3)
              (base (make-array (+ n off 3) :element-type 'character
                                           :initial-element #\z)))
         (loop for e in data for i from 0 do (setf (aref base (+ off i)) e))
         (values (make-array n :displaced-to base
                               :displaced-index-offset off
                               :element-type 'character)
                 base)))
      ((eq container 'displaced-df)
       (let* ((off 3)
              (base (make-array (+ n off 3) :element-type 'double-float
                                           :initial-element 7.0d0)))
         (loop for e in data for i from 0 do (setf (aref base (+ off i)) e))
         (values (make-array n :displaced-to base
                               :displaced-index-offset off
                               :element-type 'double-float)
                 base)))
      (t (error "unknown container ~a" container)))))

(defun %sm-extra-field (container result base)
  (cond
    ((member container '(fill fill-u8 fill-df fill-bits fill-char))
     (let ((cap (array-dimension result 0))
           (fp (fill-pointer result)))
       (format nil "F~a:~a" fp
               (%sm-enc-seq (loop for i from fp below cap collect (aref result i))))))
    ((member container '(displaced displaced-u8 displaced-bits displaced-char
                                 displaced-df))
     (let* ((n (length result))
            (cap (array-dimension base 0)))
       (format nil "B~a"
               (%sm-enc-seq (append (loop for i below 3 collect (aref base i))
                                (loop for i from (+ 3 n) below cap
                                      collect (aref base i)))))))
    (t "-")))

(defun %sm-run-sort (id op container kind pred key data)
  (let ((pc (list 0)) (kc (list 0)) (input nil) (base nil)
        (result nil) (err nil))
    (handler-case
        (multiple-value-setq (input base) (%sm-build-container container data))
      (condition (c) (setf err (type-of c))))
    (unless err
      (handler-case
          (let ((pfn (%sm-make-pred pred pc))
                (kfn (if (eq key 'none) nil (%sm-make-key key kc))))
            (setf result
                  (if (eq op 'sort)
                      (if kfn (sort input pfn :key kfn) (sort input pfn))
                      (if kfn (stable-sort input pfn :key kfn)
                          (stable-sort input pfn)))))
        (condition (c) (setf err (type-of c)))))
    (if err
        (format t "P ~a ~a ~a ~a ~a ~a SZ:~a R:ERR(~a) EQ:NA PC:~a KC:~a TY:NA PERM:NA SORTED:NA STABLE:NA X:-~%"
                id op container kind pred key (length data) (%sm-enc err)
                (car pc) (car kc))
        (let* ((rl (coerce result 'list))
               (eqv (if (eq result input) "T" "NIL"))
               (perm (if (%sm-same-multiset-p data rl) "T" "NIL"))
               (sorted (%sm-sorted-p rl pred key))
               (stable (%sm-stable-field op rl))
               (extra (%sm-extra-field container result base)))
          (format t "P ~a ~a ~a ~a ~a ~a SZ:~a R:~a EQ:~a PC:~a KC:~a TY:~a PERM:~a SORTED:~a STABLE:~a X:~a~%"
                  id op container kind pred key (length data) (%sm-enc-seq rl) eqv
                  (car pc) (car kc) (%sm-enc-type result) perm sorted stable extra)))))

(defun %sm-merge-less (pred a b)
  (case pred
    (lt (< a b))
    (gt (> a b))
    (total (%sm-total-less a b))
    (stringlt (string< a b))
    (charlt (char< a b))
    (t (error "unknown merge predicate ~a" pred))))

(defun %sm-run-merge (id rtype c1 c2 kind pred key d1 d2)
  (let ((pc (list 0)) (kc (list 0)) (s1 nil) (s2 nil)
        (result nil) (err nil))
    (handler-case
        (progn (setf s1 (%sm-build-container c1 d1))
               (setf s2 (%sm-build-container c2 d2)))
      (condition (c) (setf err (type-of c))))
    (unless err
      (handler-case
          (let ((pfn (%sm-make-pred pred pc))
                (kfn (if (eq key 'none) nil (%sm-make-key key kc))))
            (setf result (if kfn (merge rtype s1 s2 pfn :key kfn)
                             (merge rtype s1 s2 pfn))))
        (condition (c) (setf err (type-of c)))))
    (if err
        (format t "P ~a merge ~a+~a ~a ~a ~a RT:~a SZ:~a R:ERR(~a) EQ:NA PC:~a KC:~a TY:NA PERM:NA SORTED:NA STABLE:NA X:-~%"
                id c1 c2 kind pred key (%sm-enc rtype) (+ (length d1) (length d2))
                (%sm-enc err) (car pc) (car kc))
        (let* ((rl (coerce result 'list))
               (perm (if (%sm-same-multiset-p (append d1 d2) rl) "T" "NIL"))
               (sorted (%sm-sorted-p rl pred key))
               (stable (%sm-stable-field 'merge rl)))
          (format t "P ~a merge ~a+~a ~a ~a ~a RT:~a SZ:~a R:~a EQ:NA PC:~a KC:~a TY:~a PERM:~a SORTED:~a STABLE:~a X:-~%"
                  id c1 c2 kind pred key (%sm-enc rtype)
                  (+ (length d1) (length d2)) (%sm-enc-seq rl) (car pc) (car kc)
                  (%sm-enc-type result) perm sorted stable)))))

(defun %sm-nocounts (s)
  (with-output-to-string (out)
    (let ((start 0) (first t) (n (length s)))
      (loop for i from 0 to n
            when (or (= i n) (member (char s i) '(#\Space #\Newline #\Tab)))
            do (let ((tok (subseq s start i)))
                 (unless (or (zerop (length tok))
                             (and (>= (length tok) 3)
                                  (member (subseq tok 0 3) '("PC:" "KC:") :test #'string=)))
                   (if first (setf first nil) (write-char #\Space out))
                   (write-string tok out)))
               (setf start (1+ i))))))

(deftest sort-merge.probe-2
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 2 'LIST 'LIST 'SVECTOR 'STRING 'STRINGLT 'IDENTITY '("aaa" "aaa" "b" "c" "ccc") '("a" "aaa" "c"))))
  "P 2 merge LIST+SVECTOR STRING STRINGLT IDENTITY RT:YLIST SZ:8 R:[S(97),S(97,97,97),S(97,97,97),S(97,97,97),S(98),S(99),S(99),S(99,99,99)] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-5
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 5 'LIST 'DF 'SVECTOR 'DOUBLE 'LT 'COUNT 'NIL 'NIL)))
  "P 5 merge DF+SVECTOR DOUBLE LT COUNT RT:YLIST SZ:0 R:[] EQ:NA TY:YNULL PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-6
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 6 'STABLE-SORT 'BITS 'BIT 'LT 'IDENTITY '(0 1 0))))
  "P 6 STABLE-SORT BITS BIT LT IDENTITY SZ:3 R:[I0,I0,I1] EQ:T TY:(YSIMPLE-BIT-VECTOR.(I3.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-8
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 8 'SORT 'FILL-U8 'U8 'GT 'NEG '(7))))
  "P 8 SORT FILL-U8 U8 GT NEG SZ:1 R:[I7] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I4.NIL))) PERM:T SORTED:T STABLE:NA X:F1:[I255,I255,I255]")

(deftest sort-merge.probe-9
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 9 'SORT 'FILL-DF 'DOUBLE 'FALSE 'NONE 'NIL)))
  "P 9 SORT FILL-DF DOUBLE FALSE NONE SZ:0 R:[] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I3.NIL))) PERM:T SORTED:NA STABLE:NA X:F0:[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-11
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 11 'SORT 'FILL-CHAR 'CHAR 'FALSE 'COUNT 'NIL)))
  "P 11 SORT FILL-CHAR CHAR FALSE COUNT SZ:0 R:[] EQ:T TY:(YVECTOR.(YCHARACTER.(I3.NIL))) PERM:T SORTED:NA STABLE:NA X:F0:[C122,C122,C122]")

(deftest sort-merge.probe-12
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 12 'STABLE-SORT 'DISPLACED 'CONS 'FALSE 'CAR '((-4 . 0) (3 . 1) (-1 . 2) (4 . 3) (-2 . 4) (2 . 5) (2 . 6) (3 . 7)))))
  "P 12 STABLE-SORT DISPLACED CONS FALSE CAR SZ:8 R:[(I-4.I0),(I3.I1),(I-1.I2),(I4.I3),(I-2.I4),(I2.I5),(I2.I6),(I3.I7)] EQ:T TY:(YVECTOR.(T.(I8.NIL))) PERM:T SORTED:NA STABLE:T X:B[I555,I555,I555,I555,I555,I555]")

(deftest sort-merge.probe-14
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 14 'SORT 'DISPLACED-BITS 'BIT 'TRUE 'NONE 'NIL)))
  "P 14 SORT DISPLACED-BITS BIT TRUE NONE SZ:0 R:[] EQ:T TY:(YBIT-VECTOR.(I0.NIL)) PERM:T SORTED:NA STABLE:NA X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-15
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 15 'LIST 'DISPLACED 'LIST 'CONS 'GT 'CAR 'NIL '((7 . 0)))))
  "P 15 merge DISPLACED+LIST CONS GT CAR RT:YLIST SZ:1 R:[(I7.I0)] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-16
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 16 'BASE-STRING 'SVECTOR 'LIST 'CHAR 'CHARLT 'NONE 'NIL '(#\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_C))))
  "P 16 merge SVECTOR+LIST CHAR CHARLT NONE RT:YBASE-STRING SZ:3 R:[C98,C98,C99] EQ:NA TY:(YSIMPLE-BASE-STRING.(I3.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-17
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 17 'SIMPLE-VECTOR 'SVECTOR 'FILL 'FIXNUM 'TOTAL 'IDENTITY '(2 3 -2) '(2 2 3 -4 -2))))
  "P 17 merge SVECTOR+FILL FIXNUM TOTAL IDENTITY RT:YSIMPLE-VECTOR SZ:8 R:[I2,I2,I2,I3,I3,I-4,I-2,I-2] EQ:NA TY:(YSIMPLE-VECTOR.(I8.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-18
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 18 'STABLE-SORT 'SVECTOR 'STRING 'STRINGLT 'NONE 'NIL)))
  "P 18 STABLE-SORT SVECTOR STRING STRINGLT NONE SZ:0 R:[] EQ:T TY:(YSIMPLE-VECTOR.(I0.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-19
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 19 'SORT 'U8 'U8 'LT 'IDENTITY '(14 15 12))))
  "P 19 SORT U8 U8 LT IDENTITY SZ:3 R:[I12,I14,I15] EQ:T TY:(YSIMPLE-ARRAY.((YUNSIGNED-BYTE.(I8.NIL)).((I3.NIL).NIL))) PERM:T SORTED:T STABLE:NA X:-")

(deftest sort-merge.probe-20
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 20 'STABLE-SORT 'DF 'DOUBLE 'LT 'IDENTITY '(2.0d0 -0.5d0))))
  "P 20 STABLE-SORT DF DOUBLE LT IDENTITY SZ:2 R:[F4503599627370496/-53/-1,F4503599627370496/-51/1] EQ:T TY:(YSIMPLE-ARRAY.(YDOUBLE-FLOAT.((I2.NIL).NIL))) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-21
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 21 'LIST 'SVECTOR 'BITS 'BIT 'LT 'COUNT 'NIL '(0 0 0 0 1 1 1 1))))
  "P 21 merge SVECTOR+BITS BIT LT COUNT RT:YLIST SZ:8 R:[I0,I0,I0,I0,I1,I1,I1,I1] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-22
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 22 'LIST 'DISPLACED 'LIST 'RATIO 'GT 'COUNT '(1 1 3/4 1/2 1/2 0) '(3/2 1/4))))
  "P 22 merge DISPLACED+LIST RATIO GT COUNT RT:YLIST SZ:8 R:[Q3/2,I1,I1,Q3/4,Q1/2,Q1/2,Q1/4,I0] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-23
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 23 'SORT 'BITS 'BIT 'LT 'NEG 'NIL)))
  "P 23 SORT BITS BIT LT NEG SZ:0 R:[] EQ:T TY:(YSIMPLE-BIT-VECTOR.(I0.NIL)) PERM:T SORTED:T STABLE:NA X:-")

(deftest sort-merge.probe-24
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 24 'STABLE-SORT 'FILL 'FIXNUM 'LE 'COUNT 'NIL)))
  "P 24 STABLE-SORT FILL FIXNUM LE COUNT SZ:0 R:[] EQ:T TY:(YVECTOR.(T.(I3.NIL))) PERM:T SORTED:NA STABLE:T X:F0:[I777,I777,I777]")

(deftest sort-merge.probe-26
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 26 'STABLE-SORT 'FILL-DF 'DOUBLE 'LE 'IDENTITY '(3.0d0 0.0d0 2.0d0))))
  "P 26 STABLE-SORT FILL-DF DOUBLE LE IDENTITY SZ:3 R:[F0/1,F4503599627370496/-51/1,F6755399441055744/-51/1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I6.NIL))) PERM:T SORTED:NA STABLE:T X:F3:[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-28
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 28 'SORT 'FILL-CHAR 'CHAR 'CHARLT 'NONE '(#\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B))))
  "P 28 SORT FILL-CHAR CHAR CHARLT NONE SZ:8 R:[C97,C97,C97,C98,C98,C99,C99,C99] EQ:T TY:(YVECTOR.(YCHARACTER.(I11.NIL))) PERM:T SORTED:T STABLE:NA X:F8:[C122,C122,C122]")

(deftest sort-merge.probe-30
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 30 'STABLE-SORT 'DISPLACED-U8 'U8 'TOTAL 'COUNT '(12))))
  "P 30 STABLE-SORT DISPLACED-U8 U8 TOTAL COUNT SZ:1 R:[I12] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I1.NIL))) PERM:T SORTED:T STABLE:T X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-32
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 32 'STABLE-SORT 'DISPLACED-CHAR 'CHAR 'TRUE 'NONE '(#\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A))))
  "P 32 STABLE-SORT DISPLACED-CHAR CHAR TRUE NONE SZ:8 R:[C97,C99,C99,C99,C97,C98,C97,C99] EQ:T TY:(YVECTOR.(YCHARACTER.(I8.NIL))) PERM:T SORTED:NA STABLE:T X:B[C122,C122,C122,C122,C122,C122]")

(deftest sort-merge.probe-34
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 34 'STABLE-SORT 'LIST 'RATIO 'TRUE 'IDENTITY '(-1/4))))
  "P 34 STABLE-SORT LIST RATIO TRUE IDENTITY SZ:1 R:[Q-1/4] EQ:T TY:YCONS PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-35
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 35 'SORT 'SVECTOR 'STRING 'TRUE 'NONE '("a" "ccc" "a"))))
  "P 35 SORT SVECTOR STRING TRUE NONE SZ:3 R:[S(97),S(99,99,99),S(97)] EQ:T TY:(YSIMPLE-VECTOR.(I3.NIL)) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-37
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 37 'SORT 'DF 'DOUBLE 'GE 'NONE '(-0.5d0 3.0d0 -2.0d0 3.0d0 -1.0d0 2.0d0 -2.5d0))))
  "P 37 SORT DF DOUBLE GE NONE SZ:7 R:[F6755399441055744/-51/1,F6755399441055744/-51/1,F4503599627370496/-51/1,F4503599627370496/-53/-1,F4503599627370496/-52/-1,F4503599627370496/-51/-1,F5629499534213120/-51/-1] EQ:T TY:(YSIMPLE-ARRAY.(YDOUBLE-FLOAT.((I7.NIL).NIL))) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-38
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 38 'SORT 'SSTRING 'CHAR 'CHARLT 'NONE 'NIL)))
  "P 38 SORT SSTRING CHAR CHARLT NONE SZ:0 R:[] EQ:T TY:(YSIMPLE-ARRAY.(YCHARACTER.((I0.NIL).NIL))) PERM:T SORTED:T STABLE:NA X:-")

(deftest sort-merge.probe-39
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 39 'STABLE-SORT 'BSTRING 'CHAR 'FALSE 'NONE '(#\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A))))
  "P 39 STABLE-SORT BSTRING CHAR FALSE NONE SZ:2 R:[C98,C97] EQ:T TY:(YSIMPLE-BASE-STRING.(I2.NIL)) PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-40
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 40 'SORT 'BITS 'BIT 'FALSE 'NONE '(1))))
  "P 40 SORT BITS BIT FALSE NONE SZ:1 R:[I1] EQ:T TY:(YSIMPLE-BIT-VECTOR.(I1.NIL)) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-41
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 41 'SIMPLE-VECTOR 'LIST 'SVECTOR 'U8 'GT 'COUNT 'NIL 'NIL)))
  "P 41 merge LIST+SVECTOR U8 GT COUNT RT:YSIMPLE-VECTOR SZ:0 R:[] EQ:NA TY:(YSIMPLE-VECTOR.(I0.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-43
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 43 'SORT 'FILL-DF 'DOUBLE 'LT 'COUNT '(0.5d0 -2.0d0 2.0d0 -1.0d0 1.0d0 -0.5d0 2.5d0 -2.5d0))))
  "P 43 SORT FILL-DF DOUBLE LT COUNT SZ:8 R:[F5629499534213120/-51/-1,F4503599627370496/-51/-1,F4503599627370496/-52/-1,F4503599627370496/-53/-1,F4503599627370496/-53/1,F4503599627370496/-52/1,F4503599627370496/-51/1,F5629499534213120/-51/1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I11.NIL))) PERM:T SORTED:T STABLE:NA X:F8:[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-44
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 44 'LIST 'BITS 'FILL-BITS 'BIT 'LT 'IDENTITY '(0 1) 'NIL)))
  "P 44 merge BITS+FILL-BITS BIT LT IDENTITY RT:YLIST SZ:2 R:[I0,I1] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-46
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 46 'SORT 'DISPLACED 'RATIO 'LE 'IDENTITY '(-1/4 -1/2 -3/4 3/2 1/2 3/2 -1/4 3/2 0))))
  "P 46 SORT DISPLACED RATIO LE IDENTITY SZ:9 R:[Q-3/4,Q-1/2,Q-1/4,Q-1/4,I0,Q1/2,Q3/2,Q3/2,Q3/2] EQ:T TY:(YVECTOR.(T.(I9.NIL))) PERM:T SORTED:NA STABLE:NA X:B[I555,I555,I555,I555,I555,I555]")

(deftest sort-merge.probe-47
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 47 'SORT 'DISPLACED-U8 'U8 'BAD 'NONE 'NIL)))
  "P 47 SORT DISPLACED-U8 U8 BAD NONE SZ:0 R:[] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I0.NIL))) PERM:T SORTED:NA STABLE:NA X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-48
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 48 'STABLE-SORT 'DISPLACED-BITS 'BIT 'TRUE 'IDENTITY '(0))))
  "P 48 STABLE-SORT DISPLACED-BITS BIT TRUE IDENTITY SZ:1 R:[I0] EQ:T TY:(YBIT-VECTOR.(I1.NIL)) PERM:T SORTED:NA STABLE:T X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-49
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 49 'SORT 'DISPLACED-CHAR 'CHAR 'FALSE 'COUNT '(#\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C))))
  "P 49 SORT DISPLACED-CHAR CHAR FALSE COUNT SZ:2 R:[C99,C99] EQ:T TY:(YVECTOR.(YCHARACTER.(I2.NIL))) PERM:T SORTED:NA STABLE:NA X:B[C122,C122,C122,C122,C122,C122]")

(deftest sort-merge.probe-50
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 50 'SORT 'DISPLACED-DF 'DOUBLE 'GT 'NEG '(-2.0d0 1.0d0 -3.0d0 0.0d0 -3.0d0 3.0d0 -2.5d0))))
  "P 50 SORT DISPLACED-DF DOUBLE GT NEG SZ:7 R:[F6755399441055744/-51/-1,F6755399441055744/-51/-1,F5629499534213120/-51/-1,F4503599627370496/-51/-1,F0/1,F4503599627370496/-52/1,F6755399441055744/-51/1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I7.NIL))) PERM:T SORTED:T STABLE:NA X:B[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-51
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 51 'STABLE-SORT 'LIST 'DOUBLE 'GT 'NEG 'NIL)))
  "P 51 STABLE-SORT LIST DOUBLE GT NEG SZ:0 R:[] EQ:T TY:YNULL PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-53
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 53 'SORT 'U8 'U8 'LE 'COUNT '(8 1 6 7 4 13 2))))
  "P 53 SORT U8 U8 LE COUNT SZ:7 R:[I1,I2,I4,I6,I7,I8,I13] EQ:T TY:(YSIMPLE-ARRAY.((YUNSIGNED-BYTE.(I8.NIL)).((I7.NIL).NIL))) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-54
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 54 'LIST 'SVECTOR 'FILL 'FIXNUM 'GT 'IDENTITY '(4 2 -4 -4) '(3 -2 -4))))
  "P 54 merge SVECTOR+FILL FIXNUM GT IDENTITY RT:YLIST SZ:7 R:[I4,I3,I2,I-2,I-4,I-4,I-4] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-55
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 55 'SORT 'SSTRING 'CHAR 'TRUE 'IDENTITY 'NIL)))
  "P 55 SORT SSTRING CHAR TRUE IDENTITY SZ:0 R:[] EQ:T TY:(YSIMPLE-ARRAY.(YCHARACTER.((I0.NIL).NIL))) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-56
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 56 'STABLE-SORT 'BSTRING 'CHAR 'CHARLT 'COUNT '(#\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A))))
  "P 56 STABLE-SORT BSTRING CHAR CHARLT COUNT SZ:9 R:[C97,C97,C97,C97,C98,C99,C99,C99,C99] EQ:T TY:(YSIMPLE-BASE-STRING.(I9.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-59
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 59 'STABLE-SORT 'FILL-U8 'U8 'FALSE 'NEG 'NIL)))
  "P 59 STABLE-SORT FILL-U8 U8 FALSE NEG SZ:0 R:[] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I3.NIL))) PERM:T SORTED:NA STABLE:T X:F0:[I255,I255,I255]")

(deftest sort-merge.probe-60
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 60 'VECTOR 'SVECTOR 'FILL 'FIXNUM 'GT 'NONE '(-1) '(-4))))
  "P 60 merge SVECTOR+FILL FIXNUM GT NONE RT:YVECTOR SZ:2 R:[I-1,I-4] EQ:NA TY:(YSIMPLE-VECTOR.(I2.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-62
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 62 'SORT 'FILL-CHAR 'CHAR 'TRUE 'NONE 'NIL)))
  "P 62 SORT FILL-CHAR CHAR TRUE NONE SZ:0 R:[] EQ:T TY:(YVECTOR.(YCHARACTER.(I3.NIL))) PERM:T SORTED:NA STABLE:NA X:F0:[C122,C122,C122]")

(deftest sort-merge.probe-63
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 63 'SORT 'DISPLACED 'FIXNUM 'LE 'COUNT '(-2 -4))))
  "P 63 SORT DISPLACED FIXNUM LE COUNT SZ:2 R:[I-4,I-2] EQ:T TY:(YVECTOR.(T.(I2.NIL))) PERM:T SORTED:NA STABLE:NA X:B[I555,I555,I555,I555,I555,I555]")

(deftest sort-merge.probe-65
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 65 'STABLE-SORT 'DISPLACED-BITS 'BIT 'FALSE 'COUNT '(0 1 0 1 0 1 0 1 0))))
  "P 65 STABLE-SORT DISPLACED-BITS BIT FALSE COUNT SZ:9 R:[I0,I1,I0,I1,I0,I1,I0,I1,I0] EQ:T TY:(YBIT-VECTOR.(I9.NIL)) PERM:T SORTED:NA STABLE:T X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-66
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 66 'STABLE-SORT 'DISPLACED-CHAR 'CHAR 'CHARLT 'COUNT 'NIL)))
  "P 66 STABLE-SORT DISPLACED-CHAR CHAR CHARLT COUNT SZ:0 R:[] EQ:T TY:(YVECTOR.(YCHARACTER.(I0.NIL))) PERM:T SORTED:T STABLE:T X:B[C122,C122,C122,C122,C122,C122]")

(deftest sort-merge.probe-67
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 67 'SORT 'DISPLACED-DF 'DOUBLE 'FALSE 'NEG '(-0.5d0))))
  "P 67 SORT DISPLACED-DF DOUBLE FALSE NEG SZ:1 R:[F4503599627370496/-53/-1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I1.NIL))) PERM:T SORTED:NA STABLE:NA X:B[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-68
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 68 'LIST 'LIST 'DISPLACED-DF 'DOUBLE 'LT 'IDENTITY '(0.0d0) 'NIL)))
  "P 68 merge LIST+DISPLACED-DF DOUBLE LT IDENTITY RT:YLIST SZ:1 R:[F0/1] EQ:NA TY:YCONS PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-70
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 70 'SORT 'U8 'U8 'TOTAL 'COUNT '(4 13))))
  "P 70 SORT U8 U8 TOTAL COUNT SZ:2 R:[I13,I4] EQ:T TY:(YSIMPLE-ARRAY.((YUNSIGNED-BYTE.(I8.NIL)).((I2.NIL).NIL))) PERM:T SORTED:T STABLE:NA X:-")

(deftest sort-merge.probe-73
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 73 'STABLE-SORT 'BSTRING 'CHAR 'FALSE 'IDENTITY 'NIL)))
  "P 73 STABLE-SORT BSTRING CHAR FALSE IDENTITY SZ:0 R:[] EQ:T TY:(YSIMPLE-BASE-STRING.(I0.NIL)) PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-74
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 74 'STABLE-SORT 'BITS 'BIT 'FALSE 'NEG '(1 0 1 0 1 0 1))))
  "P 74 STABLE-SORT BITS BIT FALSE NEG SZ:7 R:[I1,I0,I1,I0,I1,I0,I1] EQ:T TY:(YSIMPLE-BIT-VECTOR.(I7.NIL)) PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-77
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 77 'SORT 'FILL-DF 'DOUBLE 'LE 'IDENTITY '(0.0d0 -0.5d0 3.0d0))))
  "P 77 SORT FILL-DF DOUBLE LE IDENTITY SZ:3 R:[F4503599627370496/-53/-1,F0/1,F6755399441055744/-51/1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I6.NIL))) PERM:T SORTED:NA STABLE:NA X:F3:[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-78
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 78 'SORT 'FILL-BITS 'BIT 'LT 'IDENTITY 'NIL)))
  "P 78 SORT FILL-BITS BIT LT IDENTITY SZ:0 R:[] EQ:T TY:(YBIT-VECTOR.(I3.NIL)) PERM:T SORTED:T STABLE:NA X:F0:[I1,I1,I1]")

(deftest sort-merge.probe-79
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 79 'STABLE-SORT 'FILL-CHAR 'CHAR 'TRUE 'NONE 'NIL)))
  "P 79 STABLE-SORT FILL-CHAR CHAR TRUE NONE SZ:0 R:[] EQ:T TY:(YVECTOR.(YCHARACTER.(I3.NIL))) PERM:T SORTED:NA STABLE:T X:F0:[C122,C122,C122]")

(deftest sort-merge.probe-80
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 80 'VECTOR 'DISPLACED-CHAR 'LIST 'CHAR 'CHARLT 'COUNT 'NIL '(#\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_B))))
  "P 80 merge DISPLACED-CHAR+LIST CHAR CHARLT COUNT RT:YVECTOR SZ:3 R:[C97,C98,C98] EQ:NA TY:(YSIMPLE-VECTOR.(I3.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-81
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 81 'SORT 'DISPLACED-U8 'U8 'TOTAL 'COUNT '(12))))
  "P 81 SORT DISPLACED-U8 U8 TOTAL COUNT SZ:1 R:[I12] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I1.NIL))) PERM:T SORTED:T STABLE:NA X:B[I0,I0,I0,I0,I0,I0]")

(deftest sort-merge.probe-83
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 83 'SORT 'DISPLACED-CHAR 'CHAR 'CHARLT 'NONE '(#\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A))))
  "P 83 SORT DISPLACED-CHAR CHAR CHARLT NONE SZ:7 R:[C97,C97,C97,C97,C97,C98,C98] EQ:T TY:(YVECTOR.(YCHARACTER.(I7.NIL))) PERM:T SORTED:T STABLE:NA X:B[C122,C122,C122,C122,C122,C122]")

(deftest sort-merge.probe-85
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 85 'STABLE-SORT 'LIST 'CONS 'GE 'CAR 'NIL)))
  "P 85 STABLE-SORT LIST CONS GE CAR SZ:0 R:[] EQ:T TY:YNULL PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-86
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 86 'STABLE-SORT 'SVECTOR 'CHAR 'CHARLT 'IDENTITY '(#\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_C))))
  "P 86 STABLE-SORT SVECTOR CHAR CHARLT IDENTITY SZ:7 R:[C97,C97,C97,C98,C98,C99,C99] EQ:T TY:(YSIMPLE-VECTOR.(I7.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-88
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 88 'STABLE-SORT 'DF 'DOUBLE 'FALSE 'NEG 'NIL)))
  "P 88 STABLE-SORT DF DOUBLE FALSE NEG SZ:0 R:[] EQ:T TY:(YSIMPLE-ARRAY.(YDOUBLE-FLOAT.((I0.NIL).NIL))) PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-89
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 89 'STABLE-SORT 'SSTRING 'CHAR 'TRUE 'COUNT '(#\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_B #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_B))))
  "P 89 STABLE-SORT SSTRING CHAR TRUE COUNT SZ:9 R:[C98,C97,C97,C97,C98,C99,C97,C98,C97] EQ:T TY:(YSIMPLE-ARRAY.(YCHARACTER.((I9.NIL).NIL))) PERM:T SORTED:NA STABLE:T X:-")

(deftest sort-merge.probe-90
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 90 'SIMPLE-VECTOR 'DISPLACED 'LIST 'CONS 'TOTAL 'CAR '((7 . 0)) '((1 . 1)))))
  "P 90 merge DISPLACED+LIST CONS TOTAL CAR RT:YSIMPLE-VECTOR SZ:2 R:[(I1.I1),(I7.I0)] EQ:NA TY:(YSIMPLE-VECTOR.(I2.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-91
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 91 'SORT 'BITS 'BIT 'TRUE 'NONE 'NIL)))
  "P 91 SORT BITS BIT TRUE NONE SZ:0 R:[] EQ:T TY:(YSIMPLE-BIT-VECTOR.(I0.NIL)) PERM:T SORTED:NA STABLE:NA X:-")

(deftest sort-merge.probe-93
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 93 'SORT 'FILL-U8 'U8 'BAD 'NONE 'NIL)))
  "P 93 SORT FILL-U8 U8 BAD NONE SZ:0 R:[] EQ:T TY:(YVECTOR.((YUNSIGNED-BYTE.(I8.NIL)).(I3.NIL))) PERM:T SORTED:NA STABLE:NA X:F0:[I255,I255,I255]")

(deftest sort-merge.probe-94
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 94 'SORT 'FILL-DF 'DOUBLE 'LE 'IDENTITY '(0.0d0 0.0d0 2.0d0 -1.0d0 0.5d0 -2.0d0 0.5d0))))
  "P 94 SORT FILL-DF DOUBLE LE IDENTITY SZ:7 R:[F4503599627370496/-51/-1,F4503599627370496/-52/-1,F0/1,F0/1,F4503599627370496/-53/1,F4503599627370496/-53/1,F4503599627370496/-51/1] EQ:T TY:(YVECTOR.(YDOUBLE-FLOAT.(I10.NIL))) PERM:T SORTED:NA STABLE:NA X:F7:[F7881299347898368/-50/1,F7881299347898368/-50/1,F7881299347898368/-50/1]")

(deftest sort-merge.probe-95
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 95 'VECTOR 'DISPLACED 'LIST 'CONS 'LT 'CAR '((2 . 1) (4 . 0)) 'NIL)))
  "P 95 merge DISPLACED+LIST CONS LT CAR RT:YVECTOR SZ:2 R:[(I2.I1),(I4.I0)] EQ:NA TY:(YSIMPLE-VECTOR.(I2.NIL)) PERM:T SORTED:T STABLE:T X:-")

(deftest sort-merge.probe-96
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 96 'STABLE-SORT 'FILL-CHAR 'CHAR 'FALSE 'NONE '(#\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_C #\LATIN_SMALL_LETTER_A))))
  "P 96 STABLE-SORT FILL-CHAR CHAR FALSE NONE SZ:8 R:[C99,C97,C99,C99,C99,C99,C99,C97] EQ:T TY:(YVECTOR.(YCHARACTER.(I11.NIL))) PERM:T SORTED:NA STABLE:T X:F8:[C122,C122,C122]")

(deftest sort-merge.probe-97
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-sort 97 'STABLE-SORT 'DISPLACED 'RATIO 'TRUE 'IDENTITY '(-1/4 0 -1/4 3/2 3/4 0 -1/4 1/2))))
  "P 97 STABLE-SORT DISPLACED RATIO TRUE IDENTITY SZ:8 R:[Q1/2,Q-1/4,I0,Q3/4,Q3/2,Q-1/4,I0,Q-1/4] EQ:T TY:(YVECTOR.(T.(I8.NIL))) PERM:T SORTED:NA STABLE:T X:B[I555,I555,I555,I555,I555,I555]")

(deftest sort-merge.probe-98
  (%sm-nocounts
   (with-output-to-string (*standard-output*)
     (%sm-run-merge 98 'SIMPLE-VECTOR 'LIST 'SVECTOR 'STRING 'STRINGLT 'IDENTITY '("a" "aaa" "bbb") 'NIL)))
  "P 98 merge LIST+SVECTOR STRING STRINGLT IDENTITY RT:YSIMPLE-VECTOR SZ:3 R:[S(97),S(97,97,97),S(98,98,98)] EQ:NA TY:(YSIMPLE-VECTOR.(I3.NIL)) PERM:T SORTED:T STABLE:T X:-")

;;; TYPE-OF of a displaced or fill-pointer specialised vector keeps its type.

(deftest sort-merge.type-of-displaced-u8
  (let* ((base (make-array 5 :element-type '(unsigned-byte 8)
                           :initial-contents '(9 4 1 3 2)))
         (v (make-array 3 :displaced-to base :displaced-index-offset 1
                        :element-type '(unsigned-byte 8))))
    (sort v #'<)
    (type-of v))
  (vector (unsigned-byte 8) 3))

(deftest sort-merge.type-of-displaced-bit
  (let* ((base (make-array 5 :element-type 'bit :initial-contents '(1 1 0 1 0)))
         (v (make-array 3 :displaced-to base :displaced-index-offset 1
                        :element-type 'bit)))
    (sort v #'<)
    (type-of v))
  (bit-vector 3))

(deftest sort-merge.type-of-displaced-double
  (let* ((base (make-array 5 :element-type 'double-float
                           :initial-contents '(9.0d0 4.0d0 1.0d0 3.0d0 2.0d0)))
         (v (make-array 3 :displaced-to base :displaced-index-offset 1
                        :element-type 'double-float)))
    (sort v #'<)
    (type-of v))
  (vector double-float 3))

(deftest sort-merge.type-of-fill-u8
  (let ((v (make-array 5 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 3 :initial-element 0)))
    (stable-sort v #'<)
    (type-of v))
  (vector (unsigned-byte 8) 5))

(deftest sort-merge.type-of-fill-bit
  (let ((v (make-array 5 :element-type 'bit
                              :adjustable t :fill-pointer 3 :initial-element 0)))
    (stable-sort v #'<)
    (type-of v))
  (bit-vector 5))

(deftest sort-merge.type-of-fill-double
  (let ((v (make-array 5 :element-type 'double-float
                              :adjustable t :fill-pointer 3 :initial-element 0.0d0)))
    (stable-sort v #'<)
    (type-of v))
  (vector double-float 5))
