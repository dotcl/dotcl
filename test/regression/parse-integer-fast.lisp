;;; PARSE-INTEGER with one argument is a direct call, digits are accumulated in a long (and decimal
;;; numbers longer than that are handed to BigInteger.Parse at once). Signs, white space, junk,
;;; :junk-allowed, :radix, :start / :end, numbers at the fixnum and 64-bit edges, bignums and the
;;; errors are as before; the expected values are those of the build before the change, which agree
;;; with SBCL apart from the name of the parse-error class.

(deftest parse-integer-fast.1
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345 5))

(deftest parse-integer-fast.2
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345 5))

(deftest parse-integer-fast.3
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (74565 5))

(deftest parse-integer-fast.4
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-987")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-987 4))

(deftest parse-integer-fast.5
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-987" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-987 4))

(deftest parse-integer-fast.6
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-987" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-2439 4))

(deftest parse-integer-fast.7
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+42")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (42 3))

(deftest parse-integer-fast.8
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+42" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (42 3))

(deftest parse-integer-fast.9
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+42" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (66 3))

(deftest parse-integer-fast.10
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 1))

(deftest parse-integer-fast.11
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "0" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 1))

(deftest parse-integer-fast.12
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "0" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 1))

(deftest parse-integer-fast.13
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 2))

(deftest parse-integer-fast.14
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-0" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 2))

(deftest parse-integer-fast.15
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-0" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 2))

(deftest parse-integer-fast.16
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "007")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 3))

(deftest parse-integer-fast.17
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "007" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 3))

(deftest parse-integer-fast.18
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "007" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 3))

(deftest parse-integer-fast.19
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12 ")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 4))

(deftest parse-integer-fast.20
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12 " :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 3))

(deftest parse-integer-fast.21
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12 " :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (18 4))

(deftest parse-integer-fast.22
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12 ")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 3))

(deftest parse-integer-fast.23
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12 " :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 2))

(deftest parse-integer-fast.24
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12 " :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (18 3))

(deftest parse-integer-fast.25
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 3))

(deftest parse-integer-fast.26
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 3))

(deftest parse-integer-fast.27
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " 12" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (18 3))

(deftest parse-integer-fast.28
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12a")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.29
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12a" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12 2))

(deftest parse-integer-fast.30
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12a" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (298 3))

(deftest parse-integer-fast.31
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "a12")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.32
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "a12" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 0))

(deftest parse-integer-fast.33
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "a12" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (2578 3))

(deftest parse-integer-fast.34
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.35
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 0))

(deftest parse-integer-fast.36
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.37
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " ")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.38
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " " :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 1))

(deftest parse-integer-fast.39
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER " " :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.40
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.41
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 1))

(deftest parse-integer-fast.42
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.43
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.44
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 1))

(deftest parse-integer-fast.45
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "+" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.46
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "--1")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.47
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "--1" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (NIL 1))

(deftest parse-integer-fast.48
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "--1" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.49
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1-")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.50
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1-" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 1))

(deftest parse-integer-fast.51
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1-" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.52
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775807")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775807 19))

(deftest parse-integer-fast.53
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775807" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775807 19))

(deftest parse-integer-fast.54
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775807" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (43132194673522839607303 19))

(deftest parse-integer-fast.55
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775808")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775808 19))

(deftest parse-integer-fast.56
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775808" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775808 19))

(deftest parse-integer-fast.57
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "9223372036854775808" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (43132194673522839607304 19))

(deftest parse-integer-fast.58
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775808")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775808 20))

(deftest parse-integer-fast.59
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775808" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775808 20))

(deftest parse-integer-fast.60
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775808" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-43132194673522839607304 20))

(deftest parse-integer-fast.61
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775809")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775809 20))

(deftest parse-integer-fast.62
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775809" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775809 20))

(deftest parse-integer-fast.63
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-9223372036854775809" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-43132194673522839607305 20))

(deftest parse-integer-fast.64
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387903")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (4611686018427387903 19))

(deftest parse-integer-fast.65
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387903" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (4611686018427387903 19))

(deftest parse-integer-fast.66
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387903" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (20680423093011226327299 19))

(deftest parse-integer-fast.67
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387904")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (4611686018427387904 19))

(deftest parse-integer-fast.68
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387904" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (4611686018427387904 19))

(deftest parse-integer-fast.69
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "4611686018427387904" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (20680423093011226327300 19))

(deftest parse-integer-fast.70
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "123456789012345678")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (123456789012345678 18))

(deftest parse-integer-fast.71
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "123456789012345678" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (123456789012345678 18))

(deftest parse-integer-fast.72
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "123456789012345678" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (335812727624917341816 18))

(deftest parse-integer-fast.73
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1234567890123456789")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1234567890123456789 19))

(deftest parse-integer-fast.74
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1234567890123456789" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1234567890123456789 19))

(deftest parse-integer-fast.75
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1234567890123456789" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (5373003641998677469065 19))

(deftest parse-integer-fast.76
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345678901234567890123456789012345678901234567890")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345678901234567890123456789012345678901234567890 50))

(deftest parse-integer-fast.77
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345678901234567890123456789012345678901234567890" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345678901234567890123456789012345678901234567890 50))

(deftest parse-integer-fast.78
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "12345678901234567890123456789012345678901234567890" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (114271149798383291018106828118132761521418810838475232147600 50))

(deftest parse-integer-fast.79
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-12345678901234567890123456789012345678901234567890")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-12345678901234567890123456789012345678901234567890 51))

(deftest parse-integer-fast.80
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-12345678901234567890123456789012345678901234567890" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-12345678901234567890123456789012345678901234567890 51))

(deftest parse-integer-fast.81
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-12345678901234567890123456789012345678901234567890" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-114271149798383291018106828118132761521418810838475232147600 51))

(deftest parse-integer-fast.82
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "000000000000000000000000001")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 27))

(deftest parse-integer-fast.83
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "000000000000000000000000001" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 27))

(deftest parse-integer-fast.84
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "000000000000000000000000001" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 27))

(deftest parse-integer-fast.85
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1 2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.86
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1 2" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 1))

(deftest parse-integer-fast.87
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1 2" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.88
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1,2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.89
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1,2" :JUNK-ALLOWED T)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 1))

(deftest parse-integer-fast.90
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1,2" :RADIX 16)) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PARSE-ERROR)

(deftest parse-integer-fast.91
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "xx123yy" :START 2 :END 5)) (error () (list :error)))
  (123 5))

(deftest parse-integer-fast.92
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "ff" :RADIX 16)) (error () (list :error)))
  (255 2))

(deftest parse-integer-fast.93
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "-FF" :RADIX 16)) (error () (list :error)))
  (-255 3))

(deftest parse-integer-fast.94
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "zz" :RADIX 36)) (error () (list :error)))
  (1295 2))

(deftest parse-integer-fast.95
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "ffffffffffffffffffff" :RADIX 16)) (error () (list :error)))
  (1208925819614629174706175 20))

(deftest parse-integer-fast.96
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER "1010" :RADIX 2)) (error () (list :error)))
  (10 4))

(deftest parse-integer-fast.97
  (handler-case (HANDLER-CASE (PARSE-INTEGER 5) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  TYPE-ERROR)

(deftest parse-integer-fast.98
  (handler-case (HANDLER-CASE (PARSE-INTEGER NIL) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  TYPE-ERROR)

(deftest parse-integer-fast.99
  (handler-case (HANDLER-CASE (PARSE-INTEGER "1" :RADIX 1) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  TYPE-ERROR)

(deftest parse-integer-fast.100
  (handler-case (HANDLER-CASE (PARSE-INTEGER "1" :BOGUS 1) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PROGRAM-ERROR)

(deftest parse-integer-fast.101
  (handler-case (HANDLER-CASE (EVAL (QUOTE (PARSE-INTEGER))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PROGRAM-ERROR)

(deftest parse-integer-fast.102
  (handler-case (HANDLER-CASE (EVAL (QUOTE (PARSE-INTEGER "1" "2"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  PROGRAM-ERROR)

(deftest parse-integer-fast.103
  (handler-case (LET ((S (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE CHARACTER) :INITIAL-CONTENTS "123"))) (MULTIPLE-VALUE-LIST (PARSE-INTEGER S))) (error () (list :error)))
  (123 3))

(deftest parse-integer-fast.104
  (handler-case (LET ((S (MAKE-ARRAY 5 :ELEMENT-TYPE (QUOTE CHARACTER) :FILL-POINTER 3 :INITIAL-CONTENTS "12345"))) (MULTIPLE-VALUE-LIST (PARSE-INTEGER S))) (error () (list :error)))
  (123 3))

(deftest parse-integer-fast.105
  (handler-case (MULTIPLE-VALUE-LIST (PARSE-INTEGER (COERCE "77" (QUOTE SIMPLE-BASE-STRING)))) (error () (list :error)))
  (77 2))

(deftest parse-integer-fast.106
  (handler-case (LET ((X "31")) (+ 1 (PARSE-INTEGER X))) (error () (list :error)))
  32)

(deftest parse-integer-fast.107
  (handler-case (MAPCAR (FUNCTION PARSE-INTEGER) (QUOTE ("1" "22"))) (error () (list :error)))
  (1 22))

(deftest parse-integer-fast.109
  (handler-case (FLET ((PARSE-INTEGER (S) (LIST :LOCAL S))) (PARSE-INTEGER "1")) (error () (list :error)))
  (:LOCAL "1"))

(deftest parse-integer-fast.111
  (handler-case (EQL (PARSE-INTEGER "4611686018427387903") MOST-POSITIVE-FIXNUM) (error () (list :error)))
  NIL)

(deftest parse-integer-fast.112
  (handler-case (TYPE-OF (PARSE-INTEGER "4611686018427387904")) (error () (list :error)))
  (INTEGER 4611686018427387904 4611686018427387904))

