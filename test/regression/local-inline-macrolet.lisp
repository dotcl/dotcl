;;; (declare (inline f)) on an FLET function, called from inside a MACROLET.
;;;
;;; A MACROLET between the definition and the call used to refuse the
;;; substitution outright, whatever names it bound. ironclad writes its digest
;;; rounds exactly that way: the round functions in an FLET declared INLINE, the
;;; 64 rounds unrolled by a MACROLET. A MACROLET is one more way to rebind a
;;; name, so it is checked like the others: only a macro the body itself uses
;;; refuses.

(setf dotcl:*save-sil* t)

(defun %lim-invokes (fn)
  "How many generic function calls the SIL of FN makes."
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search "LispFunction.Invoke" s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %lim-through-macrolet (a)
  (flet ((g (x) (logxor x 1)))
    (declare (inline g))
    (macrolet ((m (y) `(g ,y)))
      (+ (m a) (m a)))))

(defun %lim-unrelated-macrolet (a)
  (flet ((g (x) (logxor x 1)))
    (declare (inline g))
    (macrolet ((m () 2))
      (+ (g a) (m)))))

(defun %lim-h (x) (list :function x))

;;; The body calls %LIM-H; the MACROLET at the call site makes %LIM-H a macro.
;;; Substituted, the body would expand the call site's macro.
(defun %lim-rebound-as-macro (a)
  (flet ((g (x) (%lim-h x)))
    (declare (inline g))
    (macrolet ((%lim-h (y) `(list :macro ,y)))
      (list (g a) (%lim-h a)))))

;;; The same through a macro of the body's: its expansion names %LIM-H.
(defmacro %lim-call-h (x) `(%lim-h ,x))

(defun %lim-rebound-through-expansion (a)
  (flet ((g (x) (%lim-call-h x)))
    (declare (inline g))
    (macrolet ((%lim-h (y) `(list :macro ,y)))
      (g a))))

;;; A macro the body uses, from a MACROLET OUTSIDE the FLET: the same binding
;;; at the call site, so it substitutes and means the same thing.
(defun %lim-outer-macrolet (a)
  (macrolet ((k (y) `(* ,y 10)))
    (flet ((g (x) (k x)))
      (declare (inline g))
      (macrolet ((m (y) `(g ,y)))
        (m a)))))

(deftest local-inline-macrolet.values
  (list (%lim-through-macrolet 4)
        (%lim-unrelated-macrolet 4)
        (%lim-rebound-as-macro 1)
        (%lim-rebound-through-expansion 1)
        (%lim-outer-macrolet 3))
  (10 7 ((:function 1) (:macro 1)) (:function 1) 30))

(deftest-compiled-only local-inline-macrolet.substituted
  (list (%lim-invokes #'%lim-through-macrolet)
        (%lim-invokes #'%lim-unrelated-macrolet)
        (%lim-invokes #'%lim-outer-macrolet))
  (0 0 0))

;;; The refusals are ordinary calls: G is called, and calls %LIM-H.
(deftest-compiled-only local-inline-macrolet.refused
  (list (plusp (%lim-invokes #'%lim-rebound-as-macro))
        (plusp (%lim-invokes #'%lim-rebound-through-expansion)))
  (t t))

;;; Every call site takes a copy: there is no count of sites, only the size of
;;; the body. ironclad's 64 unrolled rounds call each round function 64 times.
(defun %lim-six-sites (a)
  (flet ((g (x) (logxor x 1)))
    (declare (inline g))
    (+ (g a) (g a) (g a) (g a) (g a) (g a))))

(defun %lim-sixty-four-sites (a)
  (flet ((g (x) (logxor x 1)))
    (declare (inline g))
    (macrolet ((rounds ()
                 `(+ ,@(loop repeat 64 collect '(g a)))))
      (rounds))))

(deftest local-inline-macrolet.many-sites-values
  (list (%lim-six-sites 4) (%lim-sixty-four-sites 4))
  (30 320))

(deftest-compiled-only local-inline-macrolet.many-sites-substituted
  (list (%lim-invokes #'%lim-six-sites)
        (%lim-invokes #'%lim-sixty-four-sites))
  (0 0))

;;; Substituted at every site, this body would be past the JIT's size limits,
;;; where the whole method is compiled without optimization. The FLET is then
;;; compiled again with *LOCAL-INLINE-CALL-LIMIT* sites per function.
(defmacro %lim-sum-of-calls (n) `(+ ,@(loop for i below n collect `(g (+ a ,i)))))

(defun %lim-past-jit-limits (a)
  (flet ((g (y) (logxor (* y 3) (ash y -2) (logand y 255) (* y y 7) (- y 11) (logior y 1))))
    (declare (inline g))
    (%lim-sum-of-calls 1500)))

(defun %lim-sil-length (fn) (length (dotcl:function-sil fn)))

(deftest local-inline-macrolet.past-jit-limits-value
  (%lim-past-jit-limits 5)
  #.(let ((s 0))
      (dotimes (i 1500 s)
        (let ((y (+ 5 i)))
          (incf s (logxor (* y 3) (ash y -2) (logand y 255) (* y y 7) (- y 11) (logior y 1)))))))

(deftest-compiled-only local-inline-macrolet.past-jit-limits-capped
  (< (%lim-sil-length #'%lim-past-jit-limits) 60000)
  t)

(setf dotcl:*save-sil* nil)
