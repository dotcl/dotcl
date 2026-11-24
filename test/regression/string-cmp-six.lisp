;;; The string comparison family has 6-arg direct entries for two bounds keyword
;;; pairs: (string= a b :start1 i :end1 j), which used to build an args array on
;;; every call. Each entry decodes the two pairs itself and falls back to the
;;; args-array path for anything else, so the two paths must agree.

(defmacro sc6-same (name fn a b k1 v1 k2 v2)
  `(deftest ,name
     (equal ,(list fn a b k1 v1 k2 v2)
            (apply ,(list 'function fn) ,a ,b (list ,k1 ,v1 ,k2 ,v2)))
     t))

(sc6-same sc6-string=-start1-end1   string= "xxabcdef" "abcdef" :start1 2 :end1 6)
(sc6-same sc6-string=-start2-end2   string= "abcdef" "xxabcdef" :start2 2 :end2 6)
(sc6-same sc6-string=-start1-start2 string= "xxabc" "yyabc" :start1 2 :start2 2)
(sc6-same sc6-string=-end1-end2     string= "abcxxx" "abcyyy" :end1 3 :end2 3)
(sc6-same sc6-string=-both-ranges   string= "xxabcxx" "yyabczz" :start1 2 :end1 5)
(sc6-same sc6-string/=              string/= "abcdef" "abcxyz" :start1 0 :end1 5)
(sc6-same sc6-string<               string< "abcdef" "abdxyz" :start1 0 :end1 4)
(sc6-same sc6-string>               string> "abdxyz" "abcdef" :start1 0 :end1 4)
(sc6-same sc6-string<=              string<= "abcxyz" "abcxyz" :start1 1 :end1 4)
(sc6-same sc6-string>=              string>= "abcxyz" "abcxyz" :start1 1 :end1 4)
(sc6-same sc6-string-equal          string-equal "ABCdef" "abcxyz" :start1 0 :end1 4)
(sc6-same sc6-string-not-equal      string-not-equal "ABCdef" "abcxyz" :start1 0 :end1 5)
(sc6-same sc6-string-lessp          string-lessp "ABCDEF" "abdxyz" :start1 0 :end1 4)
(sc6-same sc6-string-greaterp       string-greaterp "ABDXYZ" "abcdef" :start1 0 :end1 4)
(sc6-same sc6-string-not-greaterp   string-not-greaterp "ABCxyz" "abcxyz" :start1 1 :end1 4)
(sc6-same sc6-string-not-lessp      string-not-lessp "ABCxyz" "abcxyz" :start1 1 :end1 4)

(deftest sc6-values
  (list (string= "abcdef" "abcdef" :start1 0 :end1 6)
        (string= "abcdef" "abcxyz" :start1 0 :end1 6)
        (string/= "abcdef" "abcxyz" :start1 0 :end1 6)
        (string< "abc" "abd" :start1 0 :end1 3)
        (string< "abd" "abc" :start1 0 :end1 3))
  (t nil 3 2 nil))

;;; A repeated keyword keeps the first value, as the general parser does.
(deftest sc6-duplicate-keyword-first-wins
  (list (string= "abcdef" "abcdef" :start1 0 :start1 2)
        (string= "abcdef" "abcdef" :end1 4 :end1 2))
  (t nil))

(deftest sc6-symbol-and-char-designators
  (list (string= 'abc "ABCDEF" :start2 0 :end2 3)
        (string= #\a "abc" :start2 0 :end2 1)
        (string= 'abc "abcdef" :start2 1 :end2 4))
  (t t nil))

;;; Shapes the 6-arg entry must hand to the general path.
(deftest sc6-nil-bounds
  (list (string= "abc" "abc" :start1 nil :end1 nil)
        (string< "abc" "abd" :start1 nil :end1 nil))
  (t 2))

(deftest sc6-allow-other-keys-with-unknown
  (string= "abc" "abcd" :bogus 1 :allow-other-keys t)
  nil)

(deftest sc6-unknown-keyword-errors
  (handler-case (progn (string= "abc" "abc" :bogus 1 :start1 0) :no-error)
    (error () :error))
  :error)

(deftest sc6-non-integer-bound-errors
  (handler-case (progn (string= "abc" "abc" :start1 "x" :end1 1) :no-error)
    (error () :error))
  :error)

(deftest sc6-out-of-range-bound-errors
  (handler-case (progn (string= "abc" "abc" :start1 0 :end1 99) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest sc6-start-after-end-errors
  (handler-case (progn (string< "abc" "abc" :start1 2 :end1 1) :no-error)
    (type-error () :type-error))
  :type-error)

;;; Two bounds keyword pairs no longer build an args array per call.
(defun %sc6-eq-loop (a b i j n)
  (declare (simple-string a b) (fixnum i j n))
  (dotimes (k n)
    (string= a b :start1 i :end1 j)))

(defun %sc6-lt-loop (a b i j n)
  (declare (simple-string a b) (fixnum i j n))
  (dotimes (k n)
    (string< a b :start1 i :end1 j)))

(deftest-compiled-only sc6-no-per-call-allocation
  (let ((a "abcdefghij") (b "cdegzzzzzz"))
    (%sc6-eq-loop a b 2 6 1000)
    (%sc6-lt-loop a b 2 6 1000)
    (list (string= a b :start1 2 :end1 6)
          (string< a b :start1 2 :end1 6)
          (< (min-allocated-bytes (lambda () (%sc6-eq-loop a b 2 6 10000)) 3) 80000)
          (< (min-allocated-bytes (lambda () (%sc6-lt-loop a b 2 6 10000)) 3) 80000)))
  (nil 5 t t))
