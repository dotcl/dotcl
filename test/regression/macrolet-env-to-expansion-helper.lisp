;;; A MACROLET macro that hands its &ENVIRONMENT to a helper named after macro
;;; expansion (the shape of ironclad's Keccak STATE-AREF, which calls
;;; TRIVIAL-MACROEXPAND-ALL) is expanded by the analysis walks, so the typed
;;; locals around it stay raw instead of becoming boxed cells.

(setf dotcl:*save-sil* t)

(defun meh-macroexpand-all (form env)
  (macroexpand form env))

(defun meh-keccak-like (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((a 1) (b 3))
    (declare (type (unsigned-byte 64) a b))
    (macrolet ((val (x &environment env)
                 (eval (meh-macroexpand-all x env))))
      (dotimes (i n)
        (setf a (logxor a (logand #xFFFFFFFFFFFFFFFF (ash b 3)) (val 5))
              b (logand #xFFFFFFFFFFFFFFFF (+ b a)))))
    (logand #xFFFFFF (logxor a b))))

(deftest-emitting-only macrolet-env-to-expansion-helper.not-boxed
  (let ((sil (dotcl:function-sil #'meh-keccak-like)))
    (count-if (lambda (i) (and (consp i) (stringp (cadr i)) (search "Logxor2" (cadr i)))) sil))
  0)

(defun meh-plain (n)
  (let ((a 1) (b 3))
    (dotimes (i n)
      (setf a (logxor a (logand #xFFFFFFFFFFFFFFFF (ash b 3)) 5)
            b (logand #xFFFFFFFFFFFFFFFF (+ b a))))
    (logand #xFFFFFF (logxor a b))))

(deftest macrolet-env-to-expansion-helper.value
  (loop for n in '(0 1 5 100)
        always (= (meh-keccak-like n) (meh-plain n)))
  t)
