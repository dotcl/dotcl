;;; Neighbouring inputs for reader colons, WRITE :base and small rationals.

(defun %rn-both (form)
  (let ((compiled (let ((dotcl:*evaluator-mode* :compile)) (eval form)))
        (interpreted (let ((dotcl:*evaluator-mode* :interpret)) (eval form))))
    (if (equal compiled interpreted) compiled
        (list :routes-differ compiled interpreted))))

(defun %rn-kind (string)
  (handler-case
      (let ((x (read-from-string string)))
        (cond ((not (symbolp x)) (list :read x))
              ((null (symbol-package x)) (list :uninterned (symbol-name x)))
              ((eq (symbol-package x) (find-package "KEYWORD"))
               (list :keyword (symbol-name x)))
              (t (list :symbol (symbol-name x)
                       (package-name (symbol-package x))))))
    (reader-error () :reader-error)
    (error (e) (list :other-error (type-of e)))))

(defun %rn-write (object &rest keys)
  (with-output-to-string (s) (apply #'write object :stream s keys)))

(defun %rn-write-class (object &rest keys)
  (handler-case (list :value (apply #'%rn-write object keys))
    (type-error (e) (list :type-error (type-error-datum e)))))

(defun %rn-datum (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e)))))

(deftest recent-reader.escaped-colon
  (%rn-both
   '(list (%rn-kind "|a:b|")
          (%rn-kind "a\\:b:c")
          (%rn-kind "\\:a")
          (%rn-kind "a\\:")))
  ((:symbol "a:b" "COMMON-LISP-USER")
   :reader-error
   (:symbol ":A" "COMMON-LISP-USER")
   (:symbol "A:" "COMMON-LISP-USER")))

(deftest recent-reader.marker-last
  (%rn-both
   '(list (%rn-kind "cl:car:") (%rn-kind "cl::car:") (%rn-kind "a:b:c")))
  (:reader-error :reader-error :reader-error))

(deftest recent-reader.keyword-uninterned
  (%rn-both
   '(list (%rn-kind ":1") (%rn-kind "::love") (%rn-kind "#:a")
          (%rn-kind "#:a:b") (%rn-kind "#:a:b:c")))
  ((:keyword "1") (:keyword "LOVE") (:uninterned "A")
   :reader-error :reader-error))

(deftest recent-reader.preserve-whitespace
  (%rn-both
   '(list (multiple-value-list
           (read-from-string "cl:car x" t nil :preserve-whitespace t))
          (let (v p)
            (multiple-value-setq (v p)
              (read-from-string "|a:b| x" t nil :preserve-whitespace t))
            (list (symbol-name v) p))))
  ((car 6) ("a:b" 5)))

(deftest recent-reader.invert-case
  (%rn-both
   '(let ((*readtable*
           (let ((rt (copy-readtable nil)))
             (setf (readtable-case rt) :invert)
             rt)))
      (list (%rn-kind ":::love")
            (%rn-kind "cl:car")
            (%rn-kind ":1")
            (%rn-kind "|a:b|")
            (%rn-kind ":ABC")
            (%rn-kind "abc")
            (%rn-kind "a\\:b:c"))))
  (:reader-error
   (:symbol "CAR" "COMMON-LISP")
   (:keyword "1")
   (:symbol "a:b" "COMMON-LISP-USER")
   (:keyword "abc")
   (:symbol "ABC" "COMMON-LISP-USER")
   :reader-error))

(deftest recent-printer.base-rejected-datum
  (%rn-both
   '(list (%rn-write-class 35 :base 1)
          (%rn-write-class 35 :base 37)
          (%rn-write-class 35 :base 0)
          (%rn-write-class 35 :base -1)
          (%rn-write-class 35 :base 10.0)
          (%rn-write-class 35 :base nil)
          (%rn-write-class 35 :base 'a)
          (%rn-write-class 35 :base 2/3)))
  ((:type-error 1) (:type-error 37) (:type-error 0) (:type-error -1)
   (:type-error 10.0) (:type-error nil) (:type-error a) (:type-error 2/3)))

(deftest recent-printer.base-accepted-bindings
  (%rn-both
   '(list (%rn-write 35 :base 2)
          (%rn-write 35 :base 36)
          (%rn-write 35 :base 10)
          (let ((*print-base* 2)) (write-to-string 5))
          (let ((*print-base* 36)) (princ-to-string 35))
          (let ((*print-base* 2)) (prin1-to-string 35))
          (with-standard-io-syntax
            (let ((*print-base* 2)) (write-to-string 5)))
          (%rn-datum (lambda () (let ((*print-base* 1))
                                  (write-to-string 5))))
          (%rn-datum (lambda () (with-standard-io-syntax
                                  (let ((*print-base* 37))
                                    (write-to-string 5)))))
          (%rn-datum (lambda () (let ((*print-base* 1))
                                  (prin1-to-string 5))))
          (%rn-datum (lambda () (let ((*print-base* 1))
                                  (princ-to-string 5))))))
  ("100011" "Z" "35" "101" "Z" "100011" "101"
   (:type-error 1) (:type-error 37) (:type-error 1) (:type-error 1)))

(deftest recent-printer.radix-prefixes
  (%rn-both
   '(list (%rn-write 5 :base 2 :radix t)
          (%rn-write 8 :base 8 :radix t)
          (%rn-write 255 :base 16 :radix t)
          (%rn-write 35 :base 36 :radix t)
          (%rn-write 2/3 :base 36 :radix t)
          (%rn-write -35 :base 36 :radix t)
          (%rn-write -255 :base 16 :radix t)
          (%rn-write 1/2 :base 2 :radix t)))
  ("#b101" "#o10" "#xFF" "#36rZ" "#36r2/3" "#36r-Z" "#x-FF" "#b1/10"))

(deftest recent-rational.edge-parts
  (%rn-both
   '(list (+ 1/2147483647 1/2147483648)
          (+ 2147483648/2147483647 2147483647/2147483648)
          (- 2147483647/2147483646 2147483646/2147483647)
          (* 2147483647/2147483646 2147483646/2147483647)
          (/ 2147483647/2147483648 2147483648/2147483647)
          (+ 9223372036854775807/2 1/2)
          (- 9223372036854775807/2 9223372036854775806/2)
          (* 9223372036854775807/9223372036854775806
             9223372036854775806/9223372036854775807)
          (- -2147483647/2147483648 2147483647/2147483648)
          (* -2147483647/2147483648 2147483648/2147483647)
          (/ -2147483647/2147483648 2147483648/2147483647)))
  (4294967295/4611686016279904256
   9223372032559808513/4611686016279904256
   4294967293/4611686011984936962
   1
   4611686014132420609/4611686018427387904
   4611686018427387904
   1/2
   1
   -2147483647/1073741824
   -1
   -4611686014132420609/4611686018427387904))

(deftest recent-rational.normalisation-rounding-comparison
  (%rn-both
   '(let ((r (+ 1/2147483647 1/2147483648))
          (s (+ 2147483648/2147483647 2147483647/2147483648))
          (q (- 2147483647/2147483646 2147483646/2147483647))
          (u (/ 2147483647/2147483648 2147483648/2147483647)))
      (list (+ 1/3 2/3) (* 1/2 2) (- 1/2 1/2) (/ 1/3 1/6)
            (numerator r) (denominator r)
            (numerator s) (denominator s)
            (numerator q) (denominator q)
            (numerator u) (denominator u)
            (floor r) (round r) (truncate r) (rationalize r)
            (floor s) (round s) (truncate s) (rationalize s)
            (floor -1/3) (round -1/3) (truncate -1/3) (rationalize -1/3)
            (list (< 9223372036854775807/2 9223372036854775806/2)
                  (= 9223372036854775807/2 9223372036854775806/2)
                  (> 9223372036854775807/2 9223372036854775806/2))
            (list (< 9223372036854775807/9223372036854775806
                     9223372036854775806/9223372036854775805)
                  (= 9223372036854775807/9223372036854775806
                     9223372036854775806/9223372036854775805)
                  (> 9223372036854775807/9223372036854775806
                     9223372036854775806/9223372036854775805))
            (list (< 2147483647/2147483648 2147483648/2147483647)
                  (= 2147483647/2147483648 2147483648/2147483647)
                  (> 2147483647/2147483648 2147483648/2147483647)))))
  (1 1 0 2
   4294967295 4611686016279904256
   9223372032559808513 4611686016279904256
   4294967293 4611686011984936962
   4611686014132420609 4611686018427387904
   0 0 0 4294967295/4611686016279904256
   2 2 2 9223372032559808513/4611686016279904256
   -1 0 0 -1/3
   (nil nil t)
   (t nil nil)
   (t nil nil)))
