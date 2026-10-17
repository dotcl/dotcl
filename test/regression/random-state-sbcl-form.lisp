;;; A random state reads from SBCL's printed form,
;;; #S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 ...)), and draws the numbers SBCL
;;; draws from it (select's tests ship such a state). The words below are
;;; SBCL's INIT-RANDOM-STATE of seed 5489; expected values are from SBCL 2.6.8.

(defun %rss-words (seed)
  (let ((v (make-array 627 :element-type '(unsigned-byte 32))))
    (setf (aref v 0) 0 (aref v 1) #x9908B0DF (aref v 2) 624)
    (loop for i below 624
          for s = seed then (logand #xFFFFFFFF
                                    (+ (* 1812433253 (logxor s (ash s -30))) i))
          do (setf (aref v (+ 3 i)) s))
    v))

(defun %rss-state ()
  (let ((*read-eval* t))
    (read-from-string
     (format nil "#S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32) :INITIAL-CONTENTS '~S))"
             (coerce (%rss-words 5489) 'list)))))

(deftest random-state-sbcl-form.reads
  (random-state-p (%rss-state))
  t)

(deftest random-state-sbcl-form.fixnums
  (let ((s (%rss-state)))
    (list (loop repeat 5 collect (random 100 s))
          (random (expt 2 40) s)
          (random (1- (expt 2 62)) s)))
  ((92 44 95 5 97) 800187484459 1799302827895858725))

(deftest random-state-sbcl-form.bignums
  (let ((s (%rss-state)))
    (list (random (expt 2 62) s)
          (random (+ 12345 (expt 10 40)) s)
          (random (expt 2 100) s)))
  (1193941380623146742 9863200747430501942117091516896487028012
   879237734452303663375325481914))

(deftest random-state-sbcl-form.floats
  (let ((s (%rss-state)))
    (list (random 1d0 s) (random 1.0 s) (random 7.5d0 s)))
  (0.6294471125383416d0 0.81158376 5.025123073960389d0))

(deftest random-state-sbcl-form.prints-back
  (let* ((s (%rss-state))
         (s2 (with-standard-io-syntax
               (read-from-string (write-to-string s :readably t)))))
    (random 1000000 s)
    (random 1000000 s2)
    (list (subseq (with-standard-io-syntax (prin1-to-string s2)) 0 17)
          (= (random 1000000 s) (random 1000000 s2))))
  ("#S(RANDOM-STATE :" t))

(deftest random-state-sbcl-form.read-eval-nil
  (let ((*read-eval* nil))
    (handler-case (write-to-string (%rss-state) :readably t)
      (print-not-readable () :not-readable)))
  :not-readable)

(deftest random-state-sbcl-form.bad-length
  (handler-case (read-from-string "#S(RANDOM-STATE :STATE #(1 2 3))")
    (error () :error))
  :error)
