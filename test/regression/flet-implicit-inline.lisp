;;; A small FLET function is substituted at its calls without an INLINE
;;; declaration (ironclad's SHA-512 RHO has its declaration commented out).
;;; A declaring body, a NOTINLINE one, a larger one and #'F keep the function.

(setf dotcl:*save-sil* t)

(defun %fii-rho (a b)
  (flet ((ror (x s) (ldb (byte 64 0) (logior (ash x (- s)) (ash x (- 64 s)))))
         (m+ (p q) (ldb (byte 64 0) (+ p q))))
    (let ((x (m+ (logxor (ror a 14) (ror a 18) (ror a 41)) b)))
      (list (m+ x (logxor (ror b 28) (ror b 34) (ror b 39))) x))))

(defun %fii-declared (n)
  (flet ((f (k) (declare (fixnum k)) (+ k 1)))
    (f n)))

(defun %fii-notinline (n)
  (flet ((f (k) (+ k 1)))
    (declare (notinline f))
    (list (f n) (mapcar #'f '(1 2)))))

(defun %fii-as-value (n)
  (flet ((f (k) (+ k n)))
    (list (f 1) (mapcar #'f '(1 2)))))

(deftest flet-implicit-inline.values
  (list (%fii-rho #xFFFFFFFFFFFFFFFF 3)
        (%fii-rho #x6A09E667F3BCC908 #xBB67AE8584CAA73B)
        (%fii-declared 4)
        (%fii-notinline 5)
        (%fii-as-value 10))
  ((209480318978 2) (545897086836779726 16213551599857819178) 5 (6 (2 3)) (11 (11 12))))

(defun %fii-sil (fn) (%flat-text (dotcl:function-sil fn)))

(deftest-emitting-only flet-implicit-inline.no-call
  (list (search "LispFunction.Invoke" (%fii-sil #'%fii-rho))
        (and (search "LispFunction.Invoke" (%fii-sil #'%fii-notinline)) t))
  (nil t))
