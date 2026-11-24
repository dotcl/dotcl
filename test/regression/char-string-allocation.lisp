;;; The character and string functions must not allocate above the result they
;;; return. Rows are compiled DEFUNs measured with MIN-ALLOCATED-BYTES, best of
;;; five runs, 1000 calls each, inputs built outside the thunk.

(defparameter *csa-iterations* 1000)
(defparameter *csa-runs* 5)
(defparameter *csa-sink* nil)
(defparameter *csa-rows* nil)

(defparameter *csa-ss* "abcdefghABCDEFGH")
(defparameter *csa-fp* (make-array 16 :element-type 'character :fill-pointer 16
                               :initial-contents "abcdefghABCDEFGH"))
(defparameter *csa-ss-nup* "abcdefghABCDEFGH")
(defparameter *csa-fp-nup* (make-array 16 :element-type 'character :fill-pointer 16
                                   :initial-contents "abcdefghABCDEFGH"))
(defparameter *csa-chars* (coerce "abcdefghABCDEFGH" 'list))

(defmacro csa-def-row (name args &body body)
  `(defun ,name (,@args n)
     (declare (fixnum n))
     (dotimes (i n)
       (setq *csa-sink* (progn ,@body)))
     *csa-sink*))

(defmacro csa-def-pair (label variant floor-spec args &body body)
  (let* ((base (substitute #\- #\Space
                           (string-upcase (format nil "CSA-ROW-~A-~A" label variant))))
         (ss (intern (concatenate 'string base "-SS")))
         (fp (intern (concatenate 'string base "-FP"))))
    `(progn
       (csa-def-row ,ss ,args ,@body)
       (csa-def-row ,fp ,args ,@body)
       (push (list ,label "simple-string" ,variant ',floor-spec ',ss) *csa-rows*)
       (push (list ,label "char-vector-fill-pointer" ,variant ',floor-spec ',fp) *csa-rows*))))

(defmacro csa-def-char-row (label input floor-spec args &body body)
  (let ((fn (gensym (format nil "CSA-ROW-~A-" (string-upcase label)))))
    `(progn
       (csa-def-row ,fn ,args ,@body)
       (push (list ,label ,input "none" ',floor-spec ',fn) *csa-rows*))))

;;; character functions
(csa-def-char-row "CHAR-UPCASE" "character" 0 (c) (char-upcase c))
(csa-def-char-row "CHAR-DOWNCASE" "character" 0 (c) (char-downcase c))
(csa-def-char-row "CHAR-EQUAL" "character" 0 (c) (char-equal c #\A))
(csa-def-char-row "CHAR<" "character" 0 (c) (char< c #\z))
(csa-def-char-row "CHAR-CODE" "character" 0 (c) (char-code c))
(csa-def-char-row "CODE-CHAR" "integer" 0 (i) (code-char i))
(csa-def-char-row "DIGIT-CHAR-P" "character" 0 (c) (digit-char-p c))
(csa-def-char-row "CHAR-INT" "character" 0 (c) (char-int c))
(csa-def-char-row "ALPHA-CHAR-P" "character" 0 (c) (alpha-char-p c))
(csa-def-char-row "UPPER-CASE-P" "character" 0 (c) (upper-case-p c))
(csa-def-char-row "BOTH-CASE-P" "character" 0 (c) (both-case-p c))

;;; comparison family
(macrolet ((cmp (base fn)
             `(progn
                (csa-def-pair ,base "whole" 0 (a b) (,fn a b))
                (csa-def-pair ,base "start1" 0 (a b) (,fn a b :start1 1))
                (csa-def-pair ,base "end1" 0 (a b) (,fn a b :end1 15)))))
  (cmp "STRING=" STRING=)
  (cmp "STRING/=" STRING/=)
  (cmp "STRING<" STRING<)
  (cmp "STRING>" STRING>)
  (cmp "STRING<=" STRING<=)
  (cmp "STRING>=" STRING>=)
  (cmp "STRING-EQUAL" STRING-EQUAL)
  (cmp "STRING-NOT-EQUAL" STRING-NOT-EQUAL)
  (cmp "STRING-LESSP" STRING-LESSP)
  (cmp "STRING-GREATERP" STRING-GREATERP)
  (cmp "STRING-NOT-GREATERP" STRING-NOT-GREATERP)
  (cmp "STRING-NOT-LESSP" STRING-NOT-LESSP))

;;; case mapping (non-destructive)
(macrolet ((case-row (base fn)
             `(progn
                (csa-def-pair ,base "whole" (string 16) (s) (,fn s))
                (csa-def-pair ,base "start" (string 16) (s) (,fn s :start 2))
                (csa-def-pair ,base "end" (string 16) (s) (,fn s :end 14))
                (csa-def-pair ,base "start-end" (string 16) (s) (,fn s :start 2 :end 14)))))
  (case-row "STRING-UPCASE" string-upcase)
  (case-row "STRING-DOWNCASE" string-downcase)
  (case-row "STRING-CAPITALIZE" string-capitalize))

;;; case mapping (destructive)
(macrolet ((case-row (base fn)
             `(progn
                (csa-def-pair ,base "whole" 0 (s) (,fn s))
                (csa-def-pair ,base "start" 0 (s) (,fn s :start 2))
                (csa-def-pair ,base "end" 0 (s) (,fn s :end 14))
                (csa-def-pair ,base "start-end" 0 (s) (,fn s :start 2 :end 14)))))
  (case-row "NSTRING-UPCASE" nstring-upcase)
  (case-row "NSTRING-DOWNCASE" nstring-downcase)
  (case-row "NSTRING-CAPITALIZE" nstring-capitalize))

;;; STRING
(csa-def-char-row "STRING" "character" (string 1) (c) (string c))
(csa-def-char-row "STRING" "symbol" 0 (c) (string c))
(csa-def-char-row "STRING" "simple-string" 0 (c) (string c))

;;; builders
(csa-def-char-row "MAKE-STRING" "-" (string 16) (n2) (make-string n2))
(csa-def-char-row "MAKE-STRING:initial-element" "-" (string 16) (n2) (make-string n2 :initial-element #\a))
(csa-def-char-row "WITH-OUTPUT-TO-STRING" "-" (string 5) ()
  (with-output-to-string (out)
    (write-char #\a out) (write-char #\b out) (write-char #\c out)
    (write-char #\d out) (write-char #\e out)))

;;; COERCE
(csa-def-row csa-row-coerce-list-string (x) (coerce x 'string))
(csa-def-row csa-row-coerce-string-list (x) (coerce x 'list))
(csa-def-row csa-row-coerce-string-vector (x) (coerce x 'simple-vector))
(push (list "COERCE" "list" "to-string" '(string 16) 'csa-row-coerce-list-string) *csa-rows*)
(push (list "COERCE" "simple-string" "to-list" '(list 16) 'csa-row-coerce-string-list) *csa-rows*)
(push (list "COERCE" "simple-string" "to-simple-vector" '(vector 16) 'csa-row-coerce-string-vector) *csa-rows*)


;;; --- measurement ---

(defun %csa-best (thunk) (min-allocated-bytes thunk *csa-runs*))

(defun %csa-empty-loop (n) (dotimes (i n) (setq *csa-sink* nil)) *csa-sink*)

(defun %csa-mk-string (len n)
  (dotimes (i n) (setq *csa-sink* (make-string len))) *csa-sink*)
(defun %csa-mk-list (len n)
  (dotimes (i n)
    (let ((r nil))
      (dotimes (j len r) (setq r (cons #\a r)))
      (setq *csa-sink* r)))
  *csa-sink*)
(defun %csa-mk-vector (len n)
  (dotimes (i n) (setq *csa-sink* (make-array len))) *csa-sink*)

(defparameter *csa-baseline* 0)

(defun %csa-bytes-per-call (total)
  (/ (- total *csa-baseline*) (float *csa-iterations*)))

(defun %csa-floor (spec)
  (if (eql spec 0)
      0
      (let ((len (cadr spec)))
        (%csa-bytes-per-call
         (%csa-best (lambda ()
                      (case (car spec)
                        (string (%csa-mk-string len *csa-iterations*))
                        (list (%csa-mk-list len *csa-iterations*))
                        (vector (%csa-mk-vector len *csa-iterations*)))))))))

(defun %csa-measure (fn args)
  (let ((argsn (append args (list *csa-iterations*))))
    (%csa-bytes-per-call (%csa-best (lambda () (apply fn argsn))))))

(defun %csa-args-for (label input variant)
  (declare (ignore variant))
  (cond
    ((string= label "CHAR-UPCASE") (list #\a))
    ((string= label "CHAR-DOWNCASE") (list #\A))
    ((string= label "CHAR-EQUAL") (list #\a))
    ((string= label "CHAR<") (list #\a))
    ((string= label "CHAR-CODE") (list #\a))
    ((string= label "CODE-CHAR") (list 97))
    ((string= label "DIGIT-CHAR-P") (list #\5))
    ((string= label "CHAR-INT") (list #\a))
    ((string= label "ALPHA-CHAR-P") (list #\a))
    ((string= label "UPPER-CASE-P") (list #\A))
    ((string= label "BOTH-CASE-P") (list #\A))
    ((string= label "STRING")
     (cond ((string= input "character") (list #\a))
           ((string= input "symbol") (list 'abc))
           (t (list *csa-ss*))))
    ((string= label "MAKE-STRING") (list 16))
    ((string= label "MAKE-STRING:initial-element") (list 16))
    ((string= label "WITH-OUTPUT-TO-STRING") nil)
    ((string= label "COERCE")
     (cond ((string= input "list") (list *csa-chars*))
           (t (list *csa-ss*))))
    ((or (string= label "STRING-UPCASE") (string= label "STRING-DOWNCASE")
         (string= label "STRING-CAPITALIZE"))
     (list (if (string= input "simple-string") *csa-ss* *csa-fp*)))
    ((or (string= label "NSTRING-UPCASE") (string= label "NSTRING-DOWNCASE")
         (string= label "NSTRING-CAPITALIZE"))
     (list (if (string= input "simple-string") *csa-ss-nup* *csa-fp-nup*)))
    (t (list (if (string= input "simple-string") *csa-ss* *csa-fp*)
             (if (string= input "simple-string") *csa-ss* *csa-fp*)))))

(defun %csa-allocation-failures ()
  (setq *csa-baseline* (%csa-best (lambda () (%csa-empty-loop *csa-iterations*))))
  (let ((bad nil))
    (dolist (row (reverse *csa-rows*))
      (destructuring-bind (label input variant floor-spec fn) row
        ;; WITH-OUTPUT-TO-STRING has its own test: it builds a stream per call.
        (unless (string= label "WITH-OUTPUT-TO-STRING")
          (let ((got (%csa-measure fn (%csa-args-for label input variant)))
                (floor (%csa-floor floor-spec)))
            (when (> got (+ floor 8))
              (push (list label input variant got floor) bad))))))
    (nreverse bad)))

(deftest-compiled-only char-string-allocation.rows-at-floor
  (%csa-allocation-failures)
  nil)

;;; The stream-based builder is the one row left above its result-string floor.
;;; This pins its current cost (about 352 bytes per call) so it can only get
;;; cheaper; it is not asserted at the floor because no pooling is added.
(defun %csa-wos-loop (n)
  (dotimes (i n)
    (setq *csa-sink*
          (with-output-to-string (out)
            (write-char #\a out) (write-char #\b out) (write-char #\c out)
            (write-char #\d out) (write-char #\e out))))
  *csa-sink*)

(deftest-compiled-only char-string-allocation.with-output-to-string-cost
  (progn
    (setq *csa-baseline* (%csa-best (lambda () (%csa-empty-loop *csa-iterations*))))
    (<= (%csa-bytes-per-call
         (%csa-best (lambda () (%csa-wos-loop *csa-iterations*))))
        360.0))
  t)

;;; --- result equality: direct entries against the variadic FUNCALL path ---

(defun %csa-cs (&rest codes)
  (coerce (mapcar #'code-char codes) 'string))

(defparameter *csa-strings*
  (append
   (list ""
         "a"
         "ab"
         "abc"
         "abcdefgh"
         "abcdefghABCDEFGH"
         "ABCDEFGH"
         "AbCdEfGh"
         "hello, world"
         "  padded  "
         "a b c d"
         "x-y_z"
         "1234509876"
         "!?#%&"
         (%csa-cs 196 246 252 223)
         (%csa-cs 197 228 246 197)
         (%csa-cs 931 963 962 909)
         (%csa-cs 1046 1088 1080 1074 1077 1090))
   (loop for i below 30 collect (format nil "s~D-AbC-~D" i (* i 3)))
   (list (%csa-cs #xD83D #xDE00)
         (concatenate 'string "pre" (%csa-cs #xD83D #xDE00) "post"))))

(defun %csa-copy-simple (s) (copy-seq s))
(defun %csa-copy-fp (s)
  (make-array (length s) :element-type 'character
              :fill-pointer (length s) :initial-contents s))

(defun %csa-bounds (s)
  (let* ((len (length s))
         (start (min 2 len))
         (end (max start (min 14 len))))
    (values start end)))

(defmacro %csa-def-cmp (fn variant)
  (let* ((name (intern (format nil "CSA-CMP-~A-~A"
                               (string-upcase (symbol-name fn))
                               (string-upcase (symbol-name variant)))))
         (body (case variant
                 (whole `(,fn a b))
                 (start1 `(,fn a b :start1 start1))
                 (end1 `(,fn a b :end1 end1))))
         (unused (case variant
                   (whole '(start1 end1))
                   (start1 '(end1))
                   (end1 '(start1)))))
    `(defun ,name (a b start1 end1)
       (declare (ignorable ,@unused))
       ,body)))

(macrolet ((defcmp (fn)
             `(progn
                (%csa-def-cmp ,fn whole)
                (%csa-def-cmp ,fn start1)
                (%csa-def-cmp ,fn end1))))
  (defcmp string=)
  (defcmp string/=)
  (defcmp string<)
  (defcmp string>)
  (defcmp string<=)
  (defcmp string>=)
  (defcmp string-equal)
  (defcmp string-not-equal)
  (defcmp string-lessp)
  (defcmp string-greaterp)
  (defcmp string-not-greaterp)
  (defcmp string-not-lessp))

(defun %csa-cmp-equal-p (variant fn direct a b start1 end1)
  (equal (funcall direct a b start1 end1)
         (case variant
           (whole (funcall fn a b))
           (start1 (funcall fn a b :start1 start1))
           (end1 (funcall fn a b :end1 end1)))))

(deftest char-string-allocation.comparison-direct-equals-funcall
  (let ((bad nil))
    (dolist (fn '(string= string/= string< string> string<= string>=
                  string-equal string-not-equal string-lessp string-greaterp
                  string-not-greaterp string-not-lessp))
      (dolist (variant '(whole start1 end1))
        (let ((direct (intern (format nil "CSA-CMP-~A-~A"
                                      (string-upcase (symbol-name fn))
                                      (string-upcase (symbol-name variant))))))
          (dolist (maker (list #'%csa-copy-simple #'%csa-copy-fp))
            (dolist (s *csa-strings*)
              (multiple-value-bind (start1 end1) (%csa-bounds s)
                (let ((a (funcall maker s))
                      (b (funcall maker "abcdefghABCDEFGH")))
                  (unless (%csa-cmp-equal-p variant fn direct a b start1 end1)
                    (push (list fn variant s) bad)))))))))
    (nreverse bad))
  nil)

(defmacro %csa-def-case (fn variant)
  (let* ((name (intern (format nil "CSA-CASE-~A-~A"
                               (string-upcase (symbol-name fn))
                               (string-upcase (symbol-name variant)))))
         (body (case variant
                 (whole `(,fn s))
                 (start `(,fn s :start start))
                 (end `(,fn s :end end))
                 (start-end `(,fn s :start start :end end))))
         (unused (case variant
                   (whole '(start end))
                   (start '(end))
                   (end '(start))
                   (start-end '()))))
    `(defun ,name (s start end)
       (declare (ignorable ,@unused))
       ,body)))

(macrolet ((defcase (fn)
             `(progn
                (%csa-def-case ,fn whole)
                (%csa-def-case ,fn start)
                (%csa-def-case ,fn end)
                (%csa-def-case ,fn start-end))))
  (defcase string-upcase)
  (defcase string-downcase)
  (defcase string-capitalize))

(defun %csa-case-equal-p (variant fn direct s start end)
  (string= (funcall direct s start end)
           (case variant
             (whole (funcall fn s))
             (start (funcall fn s :start start))
             (end (funcall fn s :end end))
             (start-end (funcall fn s :start start :end end)))))

(deftest char-string-allocation.case-direct-equals-funcall
  (let ((bad nil))
    (dolist (fn '(string-upcase string-downcase string-capitalize))
      (dolist (variant '(whole start end start-end))
        (let ((direct (intern (format nil "CSA-CASE-~A-~A"
                                      (string-upcase (symbol-name fn))
                                      (string-upcase (symbol-name variant))))))
          (dolist (maker (list #'%csa-copy-simple #'%csa-copy-fp))
            (dolist (s *csa-strings*)
              (multiple-value-bind (start end) (%csa-bounds s)
                (let ((a (funcall maker s)))
                  (unless (%csa-case-equal-p variant fn direct a start end)
                    (push (list fn variant s) bad)))))))))
    (nreverse bad))
  nil)

(defmacro %csa-def-ncase (fn variant)
  (let* ((name (intern (format nil "CSA-NCASE-~A-~A"
                               (string-upcase (symbol-name fn))
                               (string-upcase (symbol-name variant)))))
         (body (case variant
                 (whole `(,fn s))
                 (start `(,fn s :start start))
                 (end `(,fn s :end end))
                 (start-end `(,fn s :start start :end end))))
         (unused (case variant
                   (whole '(start end))
                   (start '(end))
                   (end '(start))
                   (start-end '()))))
    `(defun ,name (s start end)
       (declare (ignorable ,@unused))
       ,body)))

(macrolet ((defncase (fn)
             `(progn
                (%csa-def-ncase ,fn whole)
                (%csa-def-ncase ,fn start)
                (%csa-def-ncase ,fn end)
                (%csa-def-ncase ,fn start-end))))
  (defncase nstring-upcase)
  (defncase nstring-downcase)
  (defncase nstring-capitalize))

(defun %csa-ncase-equal-p (variant fn direct maker s start end)
  (let* ((a (funcall maker s))
         (b (funcall maker s))
         (got (funcall direct a start end))
         (want (case variant
                 (whole (funcall fn b))
                 (start (funcall fn b :start start))
                 (end (funcall fn b :end end))
                 (start-end (funcall fn b :start start :end end)))))
    (and (string= got want) (string= a b))))

(deftest char-string-allocation.nstring-direct-equals-funcall
  (let ((bad nil))
    (dolist (fn '(nstring-upcase nstring-downcase nstring-capitalize))
      (dolist (variant '(whole start end start-end))
        (let ((direct (intern (format nil "CSA-NCASE-~A-~A"
                                      (string-upcase (symbol-name fn))
                                      (string-upcase (symbol-name variant))))))
          (dolist (maker (list #'%csa-copy-simple #'%csa-copy-fp))
            (dolist (s *csa-strings*)
              (multiple-value-bind (start end) (%csa-bounds s)
                (unless (%csa-ncase-equal-p variant fn direct maker s start end)
                  (push (list fn variant s) bad))))))))
    (nreverse bad))
  nil)

(defun %csa-make-string-direct (size ch)
  (make-string size :initial-element ch))

(defun %csa-coerce-list-string-direct (chars)
  (coerce chars 'string))

(deftest char-string-allocation.builders-direct-equal-funcall
  (let ((bad nil))
    (dolist (s *csa-strings*)
      (let* ((len (length s))
             (ch (if (> len 0) (char s 0) #\a)))
        (unless (string= (%csa-make-string-direct len ch)
                         (funcall 'make-string len :initial-element ch))
          (push (list 'make-string s) bad))
        (let ((chars (coerce s 'list)))
          (unless (string= (%csa-coerce-list-string-direct chars)
                           (funcall 'coerce chars 'string))
            (push (list 'coerce s) bad)))))
    (nreverse bad))
  nil)
