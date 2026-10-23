;;; A call to an INLINE function substituted at its call site carries the
;;; parameter types its declaimed FTYPE gives, as the out-of-line definition
;;; does: (MOD N B) on parameters the FTYPE makes small integers is computed
;;; raw, not through the generic MOD.

(setf dotcl:*save-sil* t)

(defun %ifp-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (ftype (function ((integer 1 1000) (unsigned-byte 32)) (integer 0 999)) %ifp-mod))
(declaim (inline %ifp-mod))
(defun %ifp-mod (b n) (mod n b))

(defun %ifp-call (b n) (%ifp-mod b n))

(deftest inline-ftype-params.values
  (list (%ifp-call 1000 4294967295) (%ifp-call 7 100) (%ifp-call 1 0))
  (295 2 0))

(deftest-compiled-only inline-ftype-params.raw-mod
  (%ifp-count "ModFixnumL" #'%ifp-call)
  1)
