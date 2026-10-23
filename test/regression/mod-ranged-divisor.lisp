;;; (MOD X D) / (REM X D) with a divisor that is not a constant but has a
;;; positive proven range has a proven range too, so a LET bound to it gets a
;;; raw Int64 slot and a comparison with it does not box the other operand.

(setf dotcl:*save-sil* t)

(defun %mrd-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %mrd-pick (b n)
  (declare (type (integer 1 1000) b) (type (unsigned-byte 32) n))
  (let ((th (mod (expt 2 32) b)))
    (if (>= n th) (mod n b) nil)))

(defun %mrd-small (x b)
  (declare (type (integer 0 10) x) (type (integer 1 1000) b))
  (let ((r (mod x b)))
    (+ r 1)))

(defun %mrd-rem (x b)
  (declare (type (signed-byte 32) x) (type (integer 1 1000) b))
  (let ((r (rem x b)))
    (* r 3)))

(deftest mod-ranged-divisor.values
  (list (%mrd-pick 1000 4294967295) (%mrd-pick 1000 295) (%mrd-pick 1000 296)
        (%mrd-pick 7 100) (%mrd-pick 1 0)
        (%mrd-small 10 3) (%mrd-small 10 1000) (%mrd-small 0 1)
        (%mrd-rem -2000000000 999) (%mrd-rem 2000000000 999) (%mrd-rem -5 1000))
  (295 nil 296 2 0 2 11 1 -6 6 -15))

(deftest-compiled-only mod-ranged-divisor.no-box
  (list (%mrd-count "Fixnum.Make" #'%mrd-pick)
        (%mrd-count "ModFixnumL" #'%mrd-pick))
  (1 2))
