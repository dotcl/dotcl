;;; EVENP / ODDP of an integer, LOGBITP with a small index, and ISQRT of a fixnum have their own
;;; paths. Fixnums, bignums at the fixnum edges, negative values, the type errors, a local function
;;; of the same name and the function-object forms are as before; the expected values are those of the
;;; build before the change, which agree with SBCL apart from error class names.

(deftest bignum-small-ops.1
  (handler-case (multiple-value-list (LIST (EVENP 0) (ODDP 0))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.2
  (handler-case (multiple-value-list (LIST (EVENP 1) (ODDP 1))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.3
  (handler-case (multiple-value-list (LIST (EVENP -1) (ODDP -1))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.4
  (handler-case (multiple-value-list (LIST (EVENP 2) (ODDP 2))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.5
  (handler-case (multiple-value-list (LIST (EVENP -2) (ODDP -2))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.6
  (handler-case (multiple-value-list (LIST (EVENP 3) (ODDP 3))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.7
  (handler-case (multiple-value-list (LIST (EVENP 4611686018427387903) (ODDP 4611686018427387903))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.8
  (handler-case (multiple-value-list (LIST (EVENP 4611686018427387904) (ODDP 4611686018427387904))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.9
  (handler-case (multiple-value-list (LIST (EVENP -4611686018427387904) (ODDP -4611686018427387904))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.10
  (handler-case (multiple-value-list (LIST (EVENP 9223372036854775807) (ODDP 9223372036854775807))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.11
  (handler-case (multiple-value-list (LIST (EVENP 9223372036854775808) (ODDP 9223372036854775808))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.12
  (handler-case (multiple-value-list (LIST (EVENP -9223372036854775808) (ODDP -9223372036854775808))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.13
  (handler-case (multiple-value-list (LIST (EVENP -9223372036854775809) (ODDP -9223372036854775809))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.14
  (handler-case (multiple-value-list (LIST (EVENP (EXPT 10 40)) (ODDP (EXPT 10 40)))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.15
  (handler-case (multiple-value-list (LIST (EVENP (1+ (EXPT 10 40))) (ODDP (1+ (EXPT 10 40))))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.16
  (handler-case (multiple-value-list (LIST (EVENP (- (EXPT 10 40))) (ODDP (- (EXPT 10 40))))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.17
  (handler-case (multiple-value-list (LIST (EVENP (- (1+ (EXPT 10 40)))) (ODDP (- (1+ (EXPT 10 40)))))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.18
  (handler-case (multiple-value-list (LIST (EVENP (EXPT 2 100)) (ODDP (EXPT 2 100)))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.19
  (handler-case (multiple-value-list (LIST (EVENP (1- (EXPT 2 100))) (ODDP (1- (EXPT 2 100))))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.20
  (handler-case (multiple-value-list (LOGBITP 0 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.21
  (handler-case (multiple-value-list (LOGBITP 1 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.22
  (handler-case (multiple-value-list (LOGBITP 5 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.23
  (handler-case (multiple-value-list (LOGBITP 62 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.24
  (handler-case (multiple-value-list (LOGBITP 63 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.25
  (handler-case (multiple-value-list (LOGBITP 64 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.26
  (handler-case (multiple-value-list (LOGBITP 65 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.27
  (handler-case (multiple-value-list (LOGBITP 100 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.28
  (handler-case (multiple-value-list (LOGBITP 101 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.29
  (handler-case (multiple-value-list (LOGBITP 200 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.30
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 0)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.31
  (handler-case (multiple-value-list (LOGBITP 0 1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.32
  (handler-case (multiple-value-list (LOGBITP 1 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.33
  (handler-case (multiple-value-list (LOGBITP 5 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.34
  (handler-case (multiple-value-list (LOGBITP 62 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.35
  (handler-case (multiple-value-list (LOGBITP 63 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.36
  (handler-case (multiple-value-list (LOGBITP 64 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.37
  (handler-case (multiple-value-list (LOGBITP 65 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.38
  (handler-case (multiple-value-list (LOGBITP 100 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.39
  (handler-case (multiple-value-list (LOGBITP 101 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.40
  (handler-case (multiple-value-list (LOGBITP 200 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.41
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 1)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.42
  (handler-case (multiple-value-list (LOGBITP 0 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.43
  (handler-case (multiple-value-list (LOGBITP 1 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.44
  (handler-case (multiple-value-list (LOGBITP 5 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.45
  (handler-case (multiple-value-list (LOGBITP 62 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.46
  (handler-case (multiple-value-list (LOGBITP 63 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.47
  (handler-case (multiple-value-list (LOGBITP 64 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.48
  (handler-case (multiple-value-list (LOGBITP 65 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.49
  (handler-case (multiple-value-list (LOGBITP 100 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.50
  (handler-case (multiple-value-list (LOGBITP 101 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.51
  (handler-case (multiple-value-list (LOGBITP 200 -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.52
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) -1)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.53
  (handler-case (multiple-value-list (LOGBITP 0 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.54
  (handler-case (multiple-value-list (LOGBITP 1 2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.55
  (handler-case (multiple-value-list (LOGBITP 5 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.56
  (handler-case (multiple-value-list (LOGBITP 62 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.57
  (handler-case (multiple-value-list (LOGBITP 63 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.58
  (handler-case (multiple-value-list (LOGBITP 64 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.59
  (handler-case (multiple-value-list (LOGBITP 65 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.60
  (handler-case (multiple-value-list (LOGBITP 100 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.61
  (handler-case (multiple-value-list (LOGBITP 101 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.62
  (handler-case (multiple-value-list (LOGBITP 200 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.63
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.64
  (handler-case (multiple-value-list (LOGBITP 0 -2)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.65
  (handler-case (multiple-value-list (LOGBITP 1 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.66
  (handler-case (multiple-value-list (LOGBITP 5 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.67
  (handler-case (multiple-value-list (LOGBITP 62 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.68
  (handler-case (multiple-value-list (LOGBITP 63 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.69
  (handler-case (multiple-value-list (LOGBITP 64 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.70
  (handler-case (multiple-value-list (LOGBITP 65 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.71
  (handler-case (multiple-value-list (LOGBITP 100 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.72
  (handler-case (multiple-value-list (LOGBITP 101 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.73
  (handler-case (multiple-value-list (LOGBITP 200 -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.74
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) -2)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.75
  (handler-case (multiple-value-list (LOGBITP 0 3)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.76
  (handler-case (multiple-value-list (LOGBITP 1 3)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.77
  (handler-case (multiple-value-list (LOGBITP 5 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.78
  (handler-case (multiple-value-list (LOGBITP 62 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.79
  (handler-case (multiple-value-list (LOGBITP 63 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.80
  (handler-case (multiple-value-list (LOGBITP 64 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.81
  (handler-case (multiple-value-list (LOGBITP 65 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.82
  (handler-case (multiple-value-list (LOGBITP 100 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.83
  (handler-case (multiple-value-list (LOGBITP 101 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.84
  (handler-case (multiple-value-list (LOGBITP 200 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.85
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 3)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.86
  (handler-case (multiple-value-list (LOGBITP 0 4611686018427387903)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.87
  (handler-case (multiple-value-list (LOGBITP 1 4611686018427387903)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.88
  (handler-case (multiple-value-list (LOGBITP 5 4611686018427387903)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.89
  (handler-case (multiple-value-list (LOGBITP 62 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.90
  (handler-case (multiple-value-list (LOGBITP 63 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.91
  (handler-case (multiple-value-list (LOGBITP 64 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.92
  (handler-case (multiple-value-list (LOGBITP 65 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.93
  (handler-case (multiple-value-list (LOGBITP 100 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.94
  (handler-case (multiple-value-list (LOGBITP 101 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.95
  (handler-case (multiple-value-list (LOGBITP 200 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.96
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 4611686018427387903)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.97
  (handler-case (multiple-value-list (LOGBITP 0 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.98
  (handler-case (multiple-value-list (LOGBITP 1 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.99
  (handler-case (multiple-value-list (LOGBITP 5 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.100
  (handler-case (multiple-value-list (LOGBITP 62 4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.101
  (handler-case (multiple-value-list (LOGBITP 63 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.102
  (handler-case (multiple-value-list (LOGBITP 64 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.103
  (handler-case (multiple-value-list (LOGBITP 65 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.104
  (handler-case (multiple-value-list (LOGBITP 100 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.105
  (handler-case (multiple-value-list (LOGBITP 101 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.106
  (handler-case (multiple-value-list (LOGBITP 200 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.107
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.108
  (handler-case (multiple-value-list (LOGBITP 0 -4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.109
  (handler-case (multiple-value-list (LOGBITP 1 -4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.110
  (handler-case (multiple-value-list (LOGBITP 5 -4611686018427387904)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.111
  (handler-case (multiple-value-list (LOGBITP 62 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.112
  (handler-case (multiple-value-list (LOGBITP 63 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.113
  (handler-case (multiple-value-list (LOGBITP 64 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.114
  (handler-case (multiple-value-list (LOGBITP 65 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.115
  (handler-case (multiple-value-list (LOGBITP 100 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.116
  (handler-case (multiple-value-list (LOGBITP 101 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.117
  (handler-case (multiple-value-list (LOGBITP 200 -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.118
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) -4611686018427387904)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.119
  (handler-case (multiple-value-list (LOGBITP 0 9223372036854775807)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.120
  (handler-case (multiple-value-list (LOGBITP 1 9223372036854775807)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.121
  (handler-case (multiple-value-list (LOGBITP 5 9223372036854775807)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.122
  (handler-case (multiple-value-list (LOGBITP 62 9223372036854775807)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.123
  (handler-case (multiple-value-list (LOGBITP 63 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.124
  (handler-case (multiple-value-list (LOGBITP 64 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.125
  (handler-case (multiple-value-list (LOGBITP 65 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.126
  (handler-case (multiple-value-list (LOGBITP 100 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.127
  (handler-case (multiple-value-list (LOGBITP 101 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.128
  (handler-case (multiple-value-list (LOGBITP 200 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.129
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 9223372036854775807)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.130
  (handler-case (multiple-value-list (LOGBITP 0 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.131
  (handler-case (multiple-value-list (LOGBITP 1 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.132
  (handler-case (multiple-value-list (LOGBITP 5 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.133
  (handler-case (multiple-value-list (LOGBITP 62 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.134
  (handler-case (multiple-value-list (LOGBITP 63 9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.135
  (handler-case (multiple-value-list (LOGBITP 64 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.136
  (handler-case (multiple-value-list (LOGBITP 65 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.137
  (handler-case (multiple-value-list (LOGBITP 100 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.138
  (handler-case (multiple-value-list (LOGBITP 101 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.139
  (handler-case (multiple-value-list (LOGBITP 200 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.140
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) 9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.141
  (handler-case (multiple-value-list (LOGBITP 0 -9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.142
  (handler-case (multiple-value-list (LOGBITP 1 -9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.143
  (handler-case (multiple-value-list (LOGBITP 5 -9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.144
  (handler-case (multiple-value-list (LOGBITP 62 -9223372036854775808)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.145
  (handler-case (multiple-value-list (LOGBITP 63 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.146
  (handler-case (multiple-value-list (LOGBITP 64 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.147
  (handler-case (multiple-value-list (LOGBITP 65 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.148
  (handler-case (multiple-value-list (LOGBITP 100 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.149
  (handler-case (multiple-value-list (LOGBITP 101 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.150
  (handler-case (multiple-value-list (LOGBITP 200 -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.151
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) -9223372036854775808)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.152
  (handler-case (multiple-value-list (LOGBITP 0 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.153
  (handler-case (multiple-value-list (LOGBITP 1 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.154
  (handler-case (multiple-value-list (LOGBITP 5 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.155
  (handler-case (multiple-value-list (LOGBITP 62 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.156
  (handler-case (multiple-value-list (LOGBITP 63 -9223372036854775809)) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.157
  (handler-case (multiple-value-list (LOGBITP 64 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.158
  (handler-case (multiple-value-list (LOGBITP 65 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.159
  (handler-case (multiple-value-list (LOGBITP 100 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.160
  (handler-case (multiple-value-list (LOGBITP 101 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.161
  (handler-case (multiple-value-list (LOGBITP 200 -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.162
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) -9223372036854775809)) (error () (list :error)))
  (T))

(deftest bignum-small-ops.163
  (handler-case (multiple-value-list (LOGBITP 0 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.164
  (handler-case (multiple-value-list (LOGBITP 1 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.165
  (handler-case (multiple-value-list (LOGBITP 5 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.166
  (handler-case (multiple-value-list (LOGBITP 62 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.167
  (handler-case (multiple-value-list (LOGBITP 63 (EXPT 10 40))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.168
  (handler-case (multiple-value-list (LOGBITP 64 (EXPT 10 40))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.169
  (handler-case (multiple-value-list (LOGBITP 65 (EXPT 10 40))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.170
  (handler-case (multiple-value-list (LOGBITP 100 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.171
  (handler-case (multiple-value-list (LOGBITP 101 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.172
  (handler-case (multiple-value-list (LOGBITP 200 (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.173
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (EXPT 10 40))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.174
  (handler-case (multiple-value-list (LOGBITP 0 (1+ (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.175
  (handler-case (multiple-value-list (LOGBITP 1 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.176
  (handler-case (multiple-value-list (LOGBITP 5 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.177
  (handler-case (multiple-value-list (LOGBITP 62 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.178
  (handler-case (multiple-value-list (LOGBITP 63 (1+ (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.179
  (handler-case (multiple-value-list (LOGBITP 64 (1+ (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.180
  (handler-case (multiple-value-list (LOGBITP 65 (1+ (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.181
  (handler-case (multiple-value-list (LOGBITP 100 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.182
  (handler-case (multiple-value-list (LOGBITP 101 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.183
  (handler-case (multiple-value-list (LOGBITP 200 (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.184
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (1+ (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.185
  (handler-case (multiple-value-list (LOGBITP 0 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.186
  (handler-case (multiple-value-list (LOGBITP 1 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.187
  (handler-case (multiple-value-list (LOGBITP 5 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.188
  (handler-case (multiple-value-list (LOGBITP 62 (- (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.189
  (handler-case (multiple-value-list (LOGBITP 63 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.190
  (handler-case (multiple-value-list (LOGBITP 64 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.191
  (handler-case (multiple-value-list (LOGBITP 65 (- (EXPT 10 40)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.192
  (handler-case (multiple-value-list (LOGBITP 100 (- (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.193
  (handler-case (multiple-value-list (LOGBITP 101 (- (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.194
  (handler-case (multiple-value-list (LOGBITP 200 (- (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.195
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (- (EXPT 10 40)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.196
  (handler-case (multiple-value-list (LOGBITP 0 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.197
  (handler-case (multiple-value-list (LOGBITP 1 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.198
  (handler-case (multiple-value-list (LOGBITP 5 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.199
  (handler-case (multiple-value-list (LOGBITP 62 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.200
  (handler-case (multiple-value-list (LOGBITP 63 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.201
  (handler-case (multiple-value-list (LOGBITP 64 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.202
  (handler-case (multiple-value-list (LOGBITP 65 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.203
  (handler-case (multiple-value-list (LOGBITP 100 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.204
  (handler-case (multiple-value-list (LOGBITP 101 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.205
  (handler-case (multiple-value-list (LOGBITP 200 (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.206
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (- (1+ (EXPT 10 40))))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.207
  (handler-case (multiple-value-list (LOGBITP 0 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.208
  (handler-case (multiple-value-list (LOGBITP 1 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.209
  (handler-case (multiple-value-list (LOGBITP 5 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.210
  (handler-case (multiple-value-list (LOGBITP 62 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.211
  (handler-case (multiple-value-list (LOGBITP 63 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.212
  (handler-case (multiple-value-list (LOGBITP 64 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.213
  (handler-case (multiple-value-list (LOGBITP 65 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.214
  (handler-case (multiple-value-list (LOGBITP 100 (EXPT 2 100))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.215
  (handler-case (multiple-value-list (LOGBITP 101 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.216
  (handler-case (multiple-value-list (LOGBITP 200 (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.217
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (EXPT 2 100))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.218
  (handler-case (multiple-value-list (LOGBITP 0 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.219
  (handler-case (multiple-value-list (LOGBITP 1 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.220
  (handler-case (multiple-value-list (LOGBITP 5 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.221
  (handler-case (multiple-value-list (LOGBITP 62 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.222
  (handler-case (multiple-value-list (LOGBITP 63 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.223
  (handler-case (multiple-value-list (LOGBITP 64 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.224
  (handler-case (multiple-value-list (LOGBITP 65 (1- (EXPT 2 100)))) (error () (list :error)))
  (T))

(deftest bignum-small-ops.225
  (handler-case (multiple-value-list (LOGBITP 100 (1- (EXPT 2 100)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.226
  (handler-case (multiple-value-list (LOGBITP 101 (1- (EXPT 2 100)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.227
  (handler-case (multiple-value-list (LOGBITP 200 (1- (EXPT 2 100)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.228
  (handler-case (multiple-value-list (LOGBITP (EXPT 2 40) (1- (EXPT 2 100)))) (error () (list :error)))
  (NIL))

(deftest bignum-small-ops.229
  (handler-case (multiple-value-list (LIST (ISQRT 0) (LET ((R (ISQRT 0))) (AND (<= (* R R) 0) (< 0 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((0 T)))

(deftest bignum-small-ops.230
  (handler-case (multiple-value-list (LIST (ISQRT 1) (LET ((R (ISQRT 1))) (AND (<= (* R R) 1) (< 1 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1 T)))

(deftest bignum-small-ops.231
  (handler-case (multiple-value-list (LIST (ISQRT 2) (LET ((R (ISQRT 2))) (AND (<= (* R R) 2) (< 2 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1 T)))

(deftest bignum-small-ops.232
  (handler-case (multiple-value-list (LIST (ISQRT 3) (LET ((R (ISQRT 3))) (AND (<= (* R R) 3) (< 3 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1 T)))

(deftest bignum-small-ops.233
  (handler-case (multiple-value-list (LIST (ISQRT 15) (LET ((R (ISQRT 15))) (AND (<= (* R R) 15) (< 15 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((3 T)))

(deftest bignum-small-ops.234
  (handler-case (multiple-value-list (LIST (ISQRT 16) (LET ((R (ISQRT 16))) (AND (<= (* R R) 16) (< 16 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((4 T)))

(deftest bignum-small-ops.235
  (handler-case (multiple-value-list (LIST (ISQRT 17) (LET ((R (ISQRT 17))) (AND (<= (* R R) 17) (< 17 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((4 T)))

(deftest bignum-small-ops.236
  (handler-case (multiple-value-list (LIST (ISQRT 99) (LET ((R (ISQRT 99))) (AND (<= (* R R) 99) (< 99 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((9 T)))

(deftest bignum-small-ops.237
  (handler-case (multiple-value-list (LIST (ISQRT 100) (LET ((R (ISQRT 100))) (AND (<= (* R R) 100) (< 100 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((10 T)))

(deftest bignum-small-ops.238
  (handler-case (multiple-value-list (LIST (ISQRT 1000000) (LET ((R (ISQRT 1000000))) (AND (<= (* R R) 1000000) (< 1000000 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1000 T)))

(deftest bignum-small-ops.239
  (handler-case (multiple-value-list (LIST (ISQRT 4611686018427387903) (LET ((R (ISQRT 4611686018427387903))) (AND (<= (* R R) 4611686018427387903) (< 4611686018427387903 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((2147483647 T)))

(deftest bignum-small-ops.240
  (handler-case (multiple-value-list (LIST (ISQRT 9223372036854775807) (LET ((R (ISQRT 9223372036854775807))) (AND (<= (* R R) 9223372036854775807) (< 9223372036854775807 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((3037000499 T)))

(deftest bignum-small-ops.241
  (handler-case (multiple-value-list (LIST (ISQRT 12345678901234567890) (LET ((R (ISQRT 12345678901234567890))) (AND (<= (* R R) 12345678901234567890) (< 12345678901234567890 (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((3513641828 T)))

(deftest bignum-small-ops.242
  (handler-case (multiple-value-list (LIST (ISQRT (EXPT 10 40)) (LET ((R (ISQRT (EXPT 10 40)))) (AND (<= (* R R) (EXPT 10 40)) (< (EXPT 10 40) (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((100000000000000000000 T)))

(deftest bignum-small-ops.243
  (handler-case (multiple-value-list (LIST (ISQRT (1- (EXPT 2 100))) (LET ((R (ISQRT (1- (EXPT 2 100))))) (AND (<= (* R R) (1- (EXPT 2 100))) (< (1- (EXPT 2 100)) (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1125899906842623 T)))

(deftest bignum-small-ops.244
  (handler-case (multiple-value-list (LIST (ISQRT (EXPT 2 100)) (LET ((R (ISQRT (EXPT 2 100)))) (AND (<= (* R R) (EXPT 2 100)) (< (EXPT 2 100) (* (1+ R) (1+ R))))))) (error () (list :error)))
  ((1125899906842624 T)))

(deftest bignum-small-ops.245
  (handler-case (multiple-value-list (HANDLER-CASE (EVENP 1.5) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.246
  (handler-case (multiple-value-list (HANDLER-CASE (ODDP (QUOTE A)) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.247
  (handler-case (multiple-value-list (HANDLER-CASE (EVENP 1/2) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.248
  (handler-case (multiple-value-list (HANDLER-CASE (ISQRT -1) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.249
  (handler-case (multiple-value-list (HANDLER-CASE (ISQRT 1.5) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.250
  (handler-case (multiple-value-list (HANDLER-CASE (LOGBITP -1 5) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.251
  (handler-case (multiple-value-list (HANDLER-CASE (LOGBITP 1.5 5) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.252
  (handler-case (multiple-value-list (HANDLER-CASE (LOGBITP 1 1.5) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (TYPE-ERROR))

(deftest bignum-small-ops.253
  (handler-case (multiple-value-list (LET ((X 6)) (DECLARE (FIXNUM X)) (LIST (EVENP X) (ODDP X)))) (error () (list :error)))
  ((T NIL)))

(deftest bignum-small-ops.254
  (handler-case (multiple-value-list (MAPCAR (FUNCTION EVENP) (QUOTE (1 2 3)))) (error () (list :error)))
  ((NIL T NIL)))

(deftest bignum-small-ops.255
  (handler-case (multiple-value-list (MAPCAR (FUNCTION ODDP) (LIST (EXPT 10 40) (1+ (EXPT 10 40))))) (error () (list :error)))
  ((NIL T)))

(deftest bignum-small-ops.256
  (handler-case (multiple-value-list (REMOVE-IF (FUNCTION EVENP) (QUOTE (1 2 3 4 5)))) (error () (list :error)))
  ((1 3 5)))

(deftest bignum-small-ops.257
  (handler-case (multiple-value-list (FLET ((EVENP (X) (LIST :LOCAL X))) (EVENP 3))) (error () (list :error)))
  ((:LOCAL 3)))

(deftest bignum-small-ops.258
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (EVENP))) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest bignum-small-ops.259
  (handler-case (multiple-value-list (HANDLER-CASE (EVAL (QUOTE (EVENP 1 2))) (ERROR (E) (TYPE-OF E)))) (error () (list :error)))
  (PROGRAM-ERROR))

(deftest bignum-small-ops.260
  (handler-case (multiple-value-list (ISQRT (EXPT 10 400))) (error () (list :error)))
  (100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))

