;;; Reading a decimal integer token (optional sign, up to 18 digits, *read-base* 10) takes a short
;;; route, and long decimal integers and other bases are parsed in a long before BigInteger. Signs,
;;; trailing dots, ratios, floats, radix prefixes, *read-base*, symbols that look like numbers, escapes,
;;; bignums at the 64-bit edges and the errors are as before; the expected values are those of the
;;; build before the change, which agree with SBCL.

(deftest reader-integer-tokens.1
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12345")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345 5))

(deftest reader-integer-tokens.2
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 1))

(deftest reader-integer-tokens.3
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 2))

(deftest reader-integer-tokens.4
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (5 2))

(deftest reader-integer-tokens.5
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-5 2))

(deftest reader-integer-tokens.6
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "007")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 3))

(deftest reader-integer-tokens.7
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "5.")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (5 2))

(deftest reader-integer-tokens.8
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+5.")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (5 3))

(deftest reader-integer-tokens.9
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-5.")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-5 3))

(deftest reader-integer-tokens.10
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1e3")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1000.0 3))

(deftest reader-integer-tokens.11
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1.5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.5 3))

(deftest reader-integer-tokens.12
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING ".5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0.5 2))

(deftest reader-integer-tokens.13
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-.5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-0.5 3))

(deftest reader-integer-tokens.14
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1/2 3))

(deftest reader-integer-tokens.15
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-1/2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-1/2 4))

(deftest reader-integer-tokens.16
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+1/2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1/2 4))

(deftest reader-integer-tokens.17
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/-2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|1/-2| 4))

(deftest reader-integer-tokens.18
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "4/2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (2 3))

(deftest reader-integer-tokens.19
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "0/5")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (0 3))

(deftest reader-integer-tokens.20
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "9223372036854775807")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775807 19))

(deftest reader-integer-tokens.21
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "9223372036854775808")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (9223372036854775808 19))

(deftest reader-integer-tokens.22
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-9223372036854775808")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775808 20))

(deftest reader-integer-tokens.23
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-9223372036854775809")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-9223372036854775809 20))

(deftest reader-integer-tokens.24
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "123456789012345678")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (123456789012345678 18))

(deftest reader-integer-tokens.25
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1234567890123456789")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1234567890123456789 19))

(deftest reader-integer-tokens.26
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12345678901234567890")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12345678901234567890 20))

(deftest reader-integer-tokens.27
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "123456789012345678901234567890")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (123456789012345678901234567890 30))

(deftest reader-integer-tokens.28
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-123456789012345678901234567890")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-123456789012345678901234567890 31))

(deftest reader-integer-tokens.29
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "000000000000000000001")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 21))

(deftest reader-integer-tokens.30
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "99999999999999999999999999999.")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (99999999999999999999999999999 30))

(deftest reader-integer-tokens.31
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1+")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1+ 2))

(deftest reader-integer-tokens.32
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1-")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1- 2))

(deftest reader-integer-tokens.33
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (- 1))

(deftest reader-integer-tokens.34
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (+ 1))

(deftest reader-integer-tokens.35
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "--1")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|--1| 3))

(deftest reader-integer-tokens.36
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+-1")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|+-1| 3))

(deftest reader-integer-tokens.37
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1a")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|1A| 2))

(deftest reader-integer-tokens.38
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "a1")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (A1 2))

(deftest reader-integer-tokens.39
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12abc")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (12ABC 5))

(deftest reader-integer-tokens.40
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1_000")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|1_000| 5))

(deftest reader-integer-tokens.41
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1,2")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1 1))

(deftest reader-integer-tokens.42
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "|123|")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|123| 5))

(deftest reader-integer-tokens.43
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12|3|")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|123| 5))

(deftest reader-integer-tokens.44
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "\\123")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|123| 4))

(deftest reader-integer-tokens.45
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#xFF")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (255 4))

(deftest reader-integer-tokens.46
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#x-FF")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-255 5))

(deftest reader-integer-tokens.47
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#b101")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (5 5))

(deftest reader-integer-tokens.48
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#o17")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (15 4))

(deftest reader-integer-tokens.49
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#36rZZ")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1295 6))

(deftest reader-integer-tokens.50
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "#xffffffffffffffffffff")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1208925819614629174706175 22))

(deftest reader-integer-tokens.51
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1.0d0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.0d0 5))

(deftest reader-integer-tokens.52
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1d0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.0d0 3))

(deftest reader-integer-tokens.53
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1f0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.0 3))

(deftest reader-integer-tokens.54
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1s0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.0 3))

(deftest reader-integer-tokens.55
  (handler-case (HANDLER-CASE (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12345678901234567890.0")) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1.2345679e19 22))

(deftest reader-integer-tokens.56
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (2 2))

(deftest reader-integer-tokens.57
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "7"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|7| 1))

(deftest reader-integer-tokens.58
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "ff"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (FF 2))

(deftest reader-integer-tokens.59
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "FF"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (FF 2))

(deftest reader-integer-tokens.60
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "z"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (Z 1))

(deftest reader-integer-tokens.61
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|12| 2))

(deftest reader-integer-tokens.62
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-2 3))

(deftest reader-integer-tokens.63
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+f"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (+F 2))

(deftest reader-integer-tokens.64
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10."))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10 3))

(deftest reader-integer-tokens.65
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/2"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (|1/2| 3))

(deftest reader-integer-tokens.66
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 2)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "a/b"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (A/B 3))

(deftest reader-integer-tokens.67
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (8 2))

(deftest reader-integer-tokens.68
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "7"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 1))

(deftest reader-integer-tokens.69
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "ff"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (FF 2))

(deftest reader-integer-tokens.70
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "FF"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (FF 2))

(deftest reader-integer-tokens.71
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "z"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (Z 1))

(deftest reader-integer-tokens.72
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10 2))

(deftest reader-integer-tokens.73
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-8 3))

(deftest reader-integer-tokens.74
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+f"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (+F 2))

(deftest reader-integer-tokens.75
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10."))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10 3))

(deftest reader-integer-tokens.76
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/2"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1/2 3))

(deftest reader-integer-tokens.77
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 8)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "a/b"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (A/B 3))

(deftest reader-integer-tokens.78
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (16 2))

(deftest reader-integer-tokens.79
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "7"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 1))

(deftest reader-integer-tokens.80
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "ff"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (255 2))

(deftest reader-integer-tokens.81
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "FF"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (255 2))

(deftest reader-integer-tokens.82
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "z"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (Z 1))

(deftest reader-integer-tokens.83
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (18 2))

(deftest reader-integer-tokens.84
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-16 3))

(deftest reader-integer-tokens.85
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+f"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (15 2))

(deftest reader-integer-tokens.86
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10."))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10 3))

(deftest reader-integer-tokens.87
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/2"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1/2 3))

(deftest reader-integer-tokens.88
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 16)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "a/b"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10/11 3))

(deftest reader-integer-tokens.89
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (36 2))

(deftest reader-integer-tokens.90
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "7"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (7 1))

(deftest reader-integer-tokens.91
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "ff"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (555 2))

(deftest reader-integer-tokens.92
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "FF"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (555 2))

(deftest reader-integer-tokens.93
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "z"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (35 1))

(deftest reader-integer-tokens.94
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "12"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (38 2))

(deftest reader-integer-tokens.95
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "-10"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (-36 3))

(deftest reader-integer-tokens.96
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "+f"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (15 2))

(deftest reader-integer-tokens.97
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "10."))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10 3))

(deftest reader-integer-tokens.98
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "1/2"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (1/2 3))

(deftest reader-integer-tokens.99
  (handler-case (HANDLER-CASE (LET ((*READ-BASE* 36)) (MULTIPLE-VALUE-LIST (READ-FROM-STRING "a/b"))) (ERROR (E) (TYPE-OF E))) (error () (list :error)))
  (10/11 3))

