;;; FLOOR / CEILING / TRUNCATE / ROUND of a float by 1 and ROUND of two fixnums are computed
;;; directly. The integer part and the remainder (its format, signed zeros), halves rounding to even,
;;; large and negative values, the two-fixnum edge cases and the general route are as before; the
;;; expected values are those of the build before the change, which agree with SBCL.

(deftest floor-round-fast.1
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 0.0d0)) (error () (list :error)))
  (0 0.0d0))

(deftest floor-round-fast.3
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 0.0d0)) (error () (list :error)))
  (0 0.0d0))

(deftest floor-round-fast.5
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 0.0d0)) (error () (list :error)))
  (0 0.0d0))

(deftest floor-round-fast.7
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0.0d0)) (error () (list :error)))
  (0 0.0d0))

(deftest floor-round-fast.9
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -0.0d0)) (error () (list :error)))
  (0 -0.0d0))

(deftest floor-round-fast.11
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -0.0d0)) (error () (list :error)))
  (0 -0.0d0))

(deftest floor-round-fast.13
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -0.0d0)) (error () (list :error)))
  (0 -0.0d0))

(deftest floor-round-fast.15
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -0.0d0)) (error () (list :error)))
  (0 -0.0d0))

(deftest floor-round-fast.17
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 0.3d0)) (error () (list :error)))
  (0 0.3d0))

(deftest floor-round-fast.19
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 0.3d0)) (error () (list :error)))
  (1 -0.7d0))

(deftest floor-round-fast.21
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 0.3d0)) (error () (list :error)))
  (0 0.3d0))

(deftest floor-round-fast.23
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0.3d0)) (error () (list :error)))
  (0 0.3d0))

(deftest floor-round-fast.25
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -0.3d0)) (error () (list :error)))
  (-1 0.7d0))

(deftest floor-round-fast.27
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -0.3d0)) (error () (list :error)))
  (0 -0.3d0))

(deftest floor-round-fast.29
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -0.3d0)) (error () (list :error)))
  (0 -0.3d0))

(deftest floor-round-fast.31
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -0.3d0)) (error () (list :error)))
  (0 -0.3d0))

(deftest floor-round-fast.33
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 0.5d0)) (error () (list :error)))
  (0 0.5d0))

(deftest floor-round-fast.35
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 0.5d0)) (error () (list :error)))
  (1 -0.5d0))

(deftest floor-round-fast.37
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 0.5d0)) (error () (list :error)))
  (0 0.5d0))

(deftest floor-round-fast.39
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0.5d0)) (error () (list :error)))
  (0 0.5d0))

(deftest floor-round-fast.41
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -0.5d0)) (error () (list :error)))
  (-1 0.5d0))

(deftest floor-round-fast.43
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -0.5d0)) (error () (list :error)))
  (0 -0.5d0))

(deftest floor-round-fast.45
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -0.5d0)) (error () (list :error)))
  (0 -0.5d0))

(deftest floor-round-fast.47
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -0.5d0)) (error () (list :error)))
  (0 -0.5d0))

(deftest floor-round-fast.49
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 1.5d0)) (error () (list :error)))
  (1 0.5d0))

(deftest floor-round-fast.51
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 1.5d0)) (error () (list :error)))
  (2 -0.5d0))

(deftest floor-round-fast.53
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 1.5d0)) (error () (list :error)))
  (1 0.5d0))

(deftest floor-round-fast.55
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1.5d0)) (error () (list :error)))
  (2 -0.5d0))

(deftest floor-round-fast.57
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -1.5d0)) (error () (list :error)))
  (-2 0.5d0))

(deftest floor-round-fast.59
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -1.5d0)) (error () (list :error)))
  (-1 -0.5d0))

(deftest floor-round-fast.61
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -1.5d0)) (error () (list :error)))
  (-1 -0.5d0))

(deftest floor-round-fast.63
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1.5d0)) (error () (list :error)))
  (-2 0.5d0))

(deftest floor-round-fast.65
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 2.5d0)) (error () (list :error)))
  (2 0.5d0))

(deftest floor-round-fast.67
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 2.5d0)) (error () (list :error)))
  (3 -0.5d0))

(deftest floor-round-fast.69
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 2.5d0)) (error () (list :error)))
  (2 0.5d0))

(deftest floor-round-fast.71
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 2.5d0)) (error () (list :error)))
  (2 0.5d0))

(deftest floor-round-fast.73
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -2.5d0)) (error () (list :error)))
  (-3 0.5d0))

(deftest floor-round-fast.75
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -2.5d0)) (error () (list :error)))
  (-2 -0.5d0))

(deftest floor-round-fast.77
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -2.5d0)) (error () (list :error)))
  (-2 -0.5d0))

(deftest floor-round-fast.79
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -2.5d0)) (error () (list :error)))
  (-2 -0.5d0))

(deftest floor-round-fast.81
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 3.7d0)) (error () (list :error)))
  (3 0.7000000000000002d0))

(deftest floor-round-fast.83
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 3.7d0)) (error () (list :error)))
  (4 -0.2999999999999998d0))

(deftest floor-round-fast.85
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 3.7d0)) (error () (list :error)))
  (3 0.7000000000000002d0))

(deftest floor-round-fast.87
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 3.7d0)) (error () (list :error)))
  (4 -0.2999999999999998d0))

(deftest floor-round-fast.89
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -3.7d0)) (error () (list :error)))
  (-4 0.2999999999999998d0))

(deftest floor-round-fast.91
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -3.7d0)) (error () (list :error)))
  (-3 -0.7000000000000002d0))

(deftest floor-round-fast.93
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -3.7d0)) (error () (list :error)))
  (-3 -0.7000000000000002d0))

(deftest floor-round-fast.95
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -3.7d0)) (error () (list :error)))
  (-4 0.2999999999999998d0))

(deftest floor-round-fast.97
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 1.0d15)) (error () (list :error)))
  (1000000000000000 0.0d0))

(deftest floor-round-fast.99
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 1.0d15)) (error () (list :error)))
  (1000000000000000 0.0d0))

(deftest floor-round-fast.101
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 1.0d15)) (error () (list :error)))
  (1000000000000000 0.0d0))

(deftest floor-round-fast.103
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1.0d15)) (error () (list :error)))
  (1000000000000000 0.0d0))

(deftest floor-round-fast.105
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -1.0d15)) (error () (list :error)))
  (-1000000000000000 0.0d0))

(deftest floor-round-fast.107
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -1.0d15)) (error () (list :error)))
  (-1000000000000000 0.0d0))

(deftest floor-round-fast.109
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -1.0d15)) (error () (list :error)))
  (-1000000000000000 0.0d0))

(deftest floor-round-fast.111
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1.0d15)) (error () (list :error)))
  (-1000000000000000 0.0d0))

(deftest floor-round-fast.113
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 4.611686018427388d18)) (error () (list :error)))
  (4611686018427387904 0.0d0))

(deftest floor-round-fast.115
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 4.611686018427388d18)) (error () (list :error)))
  (4611686018427387904 0.0d0))

(deftest floor-round-fast.117
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 4.611686018427388d18)) (error () (list :error)))
  (4611686018427387904 0.0d0))

(deftest floor-round-fast.119
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 4.611686018427388d18)) (error () (list :error)))
  (4611686018427387904 0.0d0))

(deftest floor-round-fast.121
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 9.3d18)) (error () (list :error)))
  (9300000000000000000 0.0d0))

(deftest floor-round-fast.123
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 9.3d18)) (error () (list :error)))
  (9300000000000000000 0.0d0))

(deftest floor-round-fast.125
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 9.3d18)) (error () (list :error)))
  (9300000000000000000 0.0d0))

(deftest floor-round-fast.127
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 9.3d18)) (error () (list :error)))
  (9300000000000000000 0.0d0))

(deftest floor-round-fast.129
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -9.3d18)) (error () (list :error)))
  (-9300000000000000000 0.0d0))

(deftest floor-round-fast.131
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -9.3d18)) (error () (list :error)))
  (-9300000000000000000 0.0d0))

(deftest floor-round-fast.133
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -9.3d18)) (error () (list :error)))
  (-9300000000000000000 0.0d0))

(deftest floor-round-fast.135
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -9.3d18)) (error () (list :error)))
  (-9300000000000000000 0.0d0))

(deftest floor-round-fast.137
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 1.0d300)) (error () (list :error)))
  (1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.139
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 1.0d300)) (error () (list :error)))
  (1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.141
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 1.0d300)) (error () (list :error)))
  (1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.143
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1.0d300)) (error () (list :error)))
  (1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.145
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -1.0d300)) (error () (list :error)))
  (-1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.147
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -1.0d300)) (error () (list :error)))
  (-1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.149
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -1.0d300)) (error () (list :error)))
  (-1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.151
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1.0d300)) (error () (list :error)))
  (-1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160 0.0d0))

(deftest floor-round-fast.161
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 0.0)) (error () (list :error)))
  (0 0.0))

(deftest floor-round-fast.163
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 0.0)) (error () (list :error)))
  (0 0.0))

(deftest floor-round-fast.165
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 0.0)) (error () (list :error)))
  (0 0.0))

(deftest floor-round-fast.167
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0.0)) (error () (list :error)))
  (0 0.0))

(deftest floor-round-fast.169
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -0.0)) (error () (list :error)))
  (0 -0.0))

(deftest floor-round-fast.171
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -0.0)) (error () (list :error)))
  (0 -0.0))

(deftest floor-round-fast.173
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -0.0)) (error () (list :error)))
  (0 -0.0))

(deftest floor-round-fast.175
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -0.0)) (error () (list :error)))
  (0 -0.0))

(deftest floor-round-fast.177
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 0.5)) (error () (list :error)))
  (0 0.5))

(deftest floor-round-fast.179
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 0.5)) (error () (list :error)))
  (1 -0.5))

(deftest floor-round-fast.181
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 0.5)) (error () (list :error)))
  (0 0.5))

(deftest floor-round-fast.183
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0.5)) (error () (list :error)))
  (0 0.5))

(deftest floor-round-fast.185
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 1.5)) (error () (list :error)))
  (1 0.5))

(deftest floor-round-fast.187
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 1.5)) (error () (list :error)))
  (2 -0.5))

(deftest floor-round-fast.189
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 1.5)) (error () (list :error)))
  (1 0.5))

(deftest floor-round-fast.191
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1.5)) (error () (list :error)))
  (2 -0.5))

(deftest floor-round-fast.193
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 2.5)) (error () (list :error)))
  (2 0.5))

(deftest floor-round-fast.195
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 2.5)) (error () (list :error)))
  (3 -0.5))

(deftest floor-round-fast.197
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 2.5)) (error () (list :error)))
  (2 0.5))

(deftest floor-round-fast.199
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 2.5)) (error () (list :error)))
  (2 0.5))

(deftest floor-round-fast.201
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -2.5)) (error () (list :error)))
  (-3 0.5))

(deftest floor-round-fast.203
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -2.5)) (error () (list :error)))
  (-2 -0.5))

(deftest floor-round-fast.205
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -2.5)) (error () (list :error)))
  (-2 -0.5))

(deftest floor-round-fast.207
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -2.5)) (error () (list :error)))
  (-2 -0.5))

(deftest floor-round-fast.209
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 3.7)) (error () (list :error)))
  (3 0.70000005))

(deftest floor-round-fast.211
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 3.7)) (error () (list :error)))
  (4 -0.29999995))

(deftest floor-round-fast.213
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 3.7)) (error () (list :error)))
  (3 0.70000005))

(deftest floor-round-fast.215
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 3.7)) (error () (list :error)))
  (4 -0.29999995))

(deftest floor-round-fast.217
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -3.7)) (error () (list :error)))
  (-4 0.29999995))

(deftest floor-round-fast.219
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -3.7)) (error () (list :error)))
  (-3 -0.70000005))

(deftest floor-round-fast.221
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -3.7)) (error () (list :error)))
  (-3 -0.70000005))

(deftest floor-round-fast.223
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -3.7)) (error () (list :error)))
  (-4 0.29999995))

(deftest floor-round-fast.225
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 1.0e10)) (error () (list :error)))
  (10000000000 0.0))

(deftest floor-round-fast.227
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 1.0e10)) (error () (list :error)))
  (10000000000 0.0))

(deftest floor-round-fast.229
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE 1.0e10)) (error () (list :error)))
  (10000000000 0.0))

(deftest floor-round-fast.231
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1.0e10)) (error () (list :error)))
  (10000000000 0.0))

(deftest floor-round-fast.233
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR -1.0e10)) (error () (list :error)))
  (-10000000000 0.0))

(deftest floor-round-fast.235
  (handler-case (MULTIPLE-VALUE-LIST (CEILING -1.0e10)) (error () (list :error)))
  (-10000000000 0.0))

(deftest floor-round-fast.237
  (handler-case (MULTIPLE-VALUE-LIST (TRUNCATE -1.0e10)) (error () (list :error)))
  (-10000000000 0.0))

(deftest floor-round-fast.239
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1.0e10)) (error () (list :error)))
  (-10000000000 0.0))

(deftest floor-round-fast.251
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 7 2)) (error () (list :error)))
  (4 -1))

(deftest floor-round-fast.252
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 5 2)) (error () (list :error)))
  (2 1))

(deftest floor-round-fast.253
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -7 2)) (error () (list :error)))
  (-4 1))

(deftest floor-round-fast.254
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -5 2)) (error () (list :error)))
  (-2 -1))

(deftest floor-round-fast.255
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 7 -2)) (error () (list :error)))
  (-4 -1))

(deftest floor-round-fast.256
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -7 -2)) (error () (list :error)))
  (4 1))

(deftest floor-round-fast.257
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 0 5)) (error () (list :error)))
  (0 0))

(deftest floor-round-fast.258
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 9 3)) (error () (list :error)))
  (3 0))

(deftest floor-round-fast.259
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1 3)) (error () (list :error)))
  (0 1))

(deftest floor-round-fast.260
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 2 3)) (error () (list :error)))
  (1 -1))

(deftest floor-round-fast.261
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1 3)) (error () (list :error)))
  (0 -1))

(deftest floor-round-fast.262
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -2 3)) (error () (list :error)))
  (-1 1))

(deftest floor-round-fast.263
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 1 2)) (error () (list :error)))
  (0 1))

(deftest floor-round-fast.264
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -1 2)) (error () (list :error)))
  (0 -1))

(deftest floor-round-fast.265
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 3 2)) (error () (list :error)))
  (2 -1))

(deftest floor-round-fast.266
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -3 2)) (error () (list :error)))
  (-2 1))

(deftest floor-round-fast.267
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 4611686018427387904 3)) (error () (list :error)))
  (1537228672809129301 1))

(deftest floor-round-fast.268
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -9223372036854775808 3)) (error () (list :error)))
  (-3074457345618258603 1))

(deftest floor-round-fast.269
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 9223372036854775807 2)) (error () (list :error)))
  (4611686018427387904 -1))

(deftest floor-round-fast.270
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 9223372036854775807 -2)) (error () (list :error)))
  (-4611686018427387904 -1))

(deftest floor-round-fast.271
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -9223372036854775808 -1)) (error () (list :error)))
  (9223372036854775808 0))

(deftest floor-round-fast.272
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -9223372036854775808 2)) (error () (list :error)))
  (-4611686018427387904 0))

(deftest floor-round-fast.273
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 100 7)) (error () (list :error)))
  (14 2))

(deftest floor-round-fast.274
  (handler-case (MULTIPLE-VALUE-LIST (ROUND -100 7)) (error () (list :error)))
  (-14 -2))

(deftest floor-round-fast.275
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 100 -7)) (error () (list :error)))
  (-14 2))

(deftest floor-round-fast.276
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 5 1)) (error () (list :error)))
  (5 0))

(deftest floor-round-fast.277
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 5 -1)) (error () (list :error)))
  (-5 0))

(deftest floor-round-fast.278
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 7 0)) (error () (list :error)))
  (:ERROR))

(deftest floor-round-fast.280
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 22/7)) (error () (list :error)))
  (3 1/7))

(deftest floor-round-fast.281
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 7/2)) (error () (list :error)))
  (4 -1/2))

(deftest floor-round-fast.282
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 5/2)) (error () (list :error)))
  (2 1/2))

(deftest floor-round-fast.283
  (handler-case (MULTIPLE-VALUE-LIST (FLOOR 3.7d0 2)) (error () (list :error)))
  (1 1.7000000000000002d0))

(deftest floor-round-fast.284
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 3.7d0 2.0d0)) (error () (list :error)))
  (2 -0.2999999999999998d0))

(deftest floor-round-fast.285
  (handler-case (MULTIPLE-VALUE-LIST (CEILING 3.2 1)) (error () (list :error)))
  (4 -0.79999995))

(deftest floor-round-fast.286
  (handler-case (MULTIPLE-VALUE-LIST (ROUND (EXPT 10 30))) (error () (list :error)))
  (1000000000000000000000000000000 0))

(deftest floor-round-fast.287
  (handler-case (MULTIPLE-VALUE-LIST (ROUND 2.5d0 1)) (error () (list :error)))
  (2 0.5d0))

(deftest floor-round-fast.288
  (handler-case (LET ((D 2.5d0)) (LIST (ROUND D) (ROUND (+ D 1)) (ROUND (- D)))) (error () (list :error)))
  (2 4 -2))

(deftest floor-round-fast.289
  (handler-case (LIST (FFLOOR 3.7d0) (FCEILING 3.2d0) (FROUND 2.5d0) (FTRUNCATE -3.7d0)) (error () (list :error)))
  (3.0d0 4.0d0 2.0d0 -3.0d0))

