;;; LOGAND / LOGIOR / LOGXOR with three or more arguments compile as the
;;; left-associated chain of two-argument calls, so (unsigned-byte 32) operands
;;; stay on the native path instead of going through an argument array.

(setf dotcl:*save-sil* t)

(defun %nlo-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %nlo-xor3 (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (ldb (byte 32 0) (ash x 3)) (ash x -7) (logand x #xff)))

(defun %nlo-maj (x y z)
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y) (logand x z) (logand y z)))

(defun %nlo-generic (a b c d) (list (logand a b c d) (logior a b c d) (logxor a b c d)))

(deftest nary-logops.values
  (list (%nlo-xor3 #xdeadbeef) (%nlo-maj #xf0f0f0f0 #xff00ff00 #x12345678)
        (%nlo-generic -1 #xff (ash 1 100) 7)
        (%nlo-generic (1- (ash 1 70)) (- (ash 1 65)) -3 12)
        (logxor 1 2 4 8 16 32 64 128 256 512)
        (logand) (logior) (logxor) (logxor 5))
  (4107316458 4063295088
   (0 -1 -1267650600228229401496703205625)
   (0 -1 1143698132569992200206)
   1023 -1 0 0 5))

(deftest nary-logops.type-error
  (handler-case (let ((x 'a)) (logxor 1 2 x)) (type-error (c) (type-error-datum c)))
  a)

(deftest-emitting-only nary-logops.native
  (list (%nlo-has "Runtime.Logxor)" #'%nlo-xor3) (%nlo-has "Runtime.Logxor)" #'%nlo-maj)
        (%nlo-has "Runtime.Logxor2)" #'%nlo-generic))
  (nil nil t))
