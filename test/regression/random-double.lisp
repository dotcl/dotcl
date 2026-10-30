;;; (RANDOM <double-float>) keeps SBCL 2.6.8's sequence on its raw-double path.

(setf dotcl:*save-sil* t)

(defun %rd-words (seed)
  (let ((v (make-array 627 :element-type '(unsigned-byte 32))))
    (setf (aref v 0) 0 (aref v 1) #x9908B0DF (aref v 2) 624)
    (loop for i below 624
          for s = seed then (logand #xFFFFFFFF
                                    (+ (* 1812433253 (logxor s (ash s -30))) i))
          do (setf (aref v (+ 3 i)) s))
    v))

(defun %rd-state ()
  (let ((*read-eval* t))
    (read-from-string
     (format nil "#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32) :INITIAL-CONTENTS '~S))"
             (coerce (%rd-words 5489) 'list)))))

(defun %rd-loop (s n limit)
  (declare (fixnum n) (double-float limit))
  (let ((y 0d0))
    (dotimes (i n)
      (setf y (random limit s)))
    (* 1d0 y)))

(defun %rd-loop-implicit (n)
  (declare (fixnum n))
  (let ((y 0d0))
    (dotimes (i n)
      (setf y (random 1d0)))
    (* 1d0 y)))

(deftest random-double.loop-with-state
  (let ((s1 (%rd-state))
        (s2 (%rd-state)))
    (list (%rd-loop s1 8 1d0)
          (%rd-loop s2 8 7.5d0)))
  (0.09376334465151004d0 0.7032250848863253d0))

(deftest random-double.loop-implicit-state
  (let ((*random-state* (%rd-state)))
    (%rd-loop-implicit 8))
  0.09376334465151004d0)

(deftest random-double.result-type
  (let ((s (%rd-state)))
    (list (type-of (random 1d0 s)) (type-of (random 7.5d0 s))))
  (double-float double-float))

(deftest random-double.non-positive
  (list (handler-case (the double-float (random -1d0)) (type-error () :type-error))
        (handler-case (the double-float (random 0d0)) (type-error () :type-error)))
  (:type-error :type-error))

(deftest-compiled-only random-double.no-allocation
  (< (min-allocated-bytes (lambda () (%rd-loop-implicit 100000))) 10000)
  t)

(deftest-emitting-only random-double.raw-entry
  (let ((sil (%flat-text (dotcl:function-sil #'%rd-loop))))
    (list (and (search "Runtime.RandomDoubleRaw2" sil) t)
          (and (search "Runtime.Random\"" sil) t)))
  (t nil))

(setf dotcl:*save-sil* nil)
