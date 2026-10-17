;;; LDB / DPB with a byte specifier known only at run time: fixnum fields of
;;; fixnums on the fast entry, the rest by the general definition.

(declaim (notinline %ldrs-spec))
(defun %ldrs-spec (s p) (byte s p))
(defun %ldrs-ldb (s p x) (ldb (%ldrs-spec s p) x))
(defun %ldrs-dpb (v s p x) (dpb v (%ldrs-spec s p) x))

(deftest ldb-dpb-runtime-spec.ldb
  (list (%ldrs-ldb 0 0 -1)
        (%ldrs-ldb 32 0 #x123456789)
        (%ldrs-ldb 8 4 -1)
        (%ldrs-ldb 5 59 (1- (expt 2 63)))
        (%ldrs-ldb 10 70 -1)
        (%ldrs-ldb 10 70 12345)
        (%ldrs-ldb 62 0 -1)
        (%ldrs-ldb 63 0 -1)
        (%ldrs-ldb 64 0 -1)
        (%ldrs-ldb 70 3 (- (expt 2 63)))
        (%ldrs-ldb 64 0 (1- (expt 2 63)))
        (%ldrs-ldb 37 27 (1- (expt 2 64)))
        (%ldrs-ldb 3 1 (expt 2 100))
        (byte-size (%ldrs-spec 7 9))
        (byte-position (%ldrs-spec 7 9))
        (handler-case (%ldrs-ldb 4 0 1.5) (type-error () :type-error)))
  (0 591751049 255 15 1023 0 4611686018427387903 9223372036854775807
   18446744073709551615 1179438699212804456448 9223372036854775807 137438953471 0
   7 9 :type-error))

(deftest ldb-dpb-runtime-spec.dpb
  (list (%ldrs-dpb 31 5 59 0)
        (%ldrs-dpb -1 4 4 0)
        (%ldrs-dpb 0 4 4 -1)
        (%ldrs-dpb 5 3 60 -1)
        (%ldrs-dpb 1 1 63 0)
        (%ldrs-dpb 1 2 62 0)
        (%ldrs-dpb #xAB 8 56 #x0123456789ABCDEF)
        (%ldrs-dpb (expt 2 70) 8 0 3)
        (%ldrs-dpb 3 2 0 (expt 2 70))
        (%ldrs-dpb 7 0 5 9))
  (17870283321406128128 240 -241 -2305843009213693953 9223372036854775808
   4611686018427387904 12331776515664236015 0 1180591620717411303427 9))
