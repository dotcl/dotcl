;;; (ASH x (THE FIXNUM n)) with a literal n is the shift by n: macros that wrap every operand in THE
;;; (closure-common's %ASH) no longer fall back to the generic ASH.

(setf dotcl:*save-sil* t)

(defun %atc-right (a) (declare (fixnum a)) (the fixnum (ash (the fixnum a) (the fixnum -57))))
(defun %atc-left (a) (declare (fixnum a)) (the fixnum (ash (the fixnum (logand a 144115188075855871)) (the fixnum 5))))
(defun %atc-plain (a) (declare (fixnum a)) (the fixnum (ash a -3)))

(deftest ash-the-count.values
  (list (%atc-right (expt 2 60)) (%atc-right -1) (%atc-right 12345)
        (%atc-left 3) (%atc-left (1- (expt 2 62))) (%atc-left -1)
        (%atc-plain 80) (%atc-plain -80))
  (8 -1 0 96 4611686018427387872 4611686018427387872 10 -10))

(deftest-emitting-only ash-the-count.no-generic-call
  (flet ((generic-p (fn) (and (search "runtime.ash\"" (string-downcase (format nil "~S" (dotcl:function-sil fn)))) t)))
    (list (generic-p #'%atc-right) (generic-p #'%atc-plain)))
  (nil nil))
