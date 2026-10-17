;;; The values of an UNWIND-PROTECT protected form and of a MULTIPLE-VALUE-PROG1
;;; first form, held while the cleanup / the other forms run, which publish
;;; values of their own. Expected values are SBCL's.

(defun %mhv-n (n)
  (case n
    (0 (values))
    (1 :a)
    (2 (values :a :b))
    (3 (values :a :b :c))
    (4 (values :a :b :c :d))
    (t (values-list (subseq '(:a :b :c :d :e :f :g) 0 n)))))
(defun %mhv-noise () (values 9 8 7 6 5))

(defun %mhv-p1 (n) (multiple-value-prog1 (%mhv-n n) (%mhv-noise) (values 1 2)))
(defun %mhv-up (n) (unwind-protect (%mhv-n n) (%mhv-noise) (values 1 2)))
(defun %mhv-p1-lit (x) (multiple-value-prog1 (values x 2 3) (%mhv-noise)))
(defun %mhv-up-lit (x) (unwind-protect (values x 2) (%mhv-noise)))

(deftest mv-held-values.counts
  (loop for n in '(0 1 2 3 4 5 7)
        collect (list (multiple-value-list (%mhv-p1 n))
                      (multiple-value-list (%mhv-up n))))
  ((() ()) ((:a) (:a)) ((:a :b) (:a :b)) ((:a :b :c) (:a :b :c))
   ((:a :b :c :d) (:a :b :c :d)) ((:a :b :c :d :e) (:a :b :c :d :e))
   ((:a :b :c :d :e :f :g) (:a :b :c :d :e :f :g))))

;; The callers pass the value modes: primary only, captured by
;; MULTIPLE-VALUE-BIND / NTH-VALUE, none (MULTIPLE-VALUE-LIST, FUNCALL, APPLY).
(deftest mv-held-values.caller-modes
  (list (%mhv-p1 3) (%mhv-up 3) (%mhv-p1 0) (%mhv-up 0)
        (multiple-value-bind (a b c d e) (%mhv-p1 4) (list a b c d e))
        (multiple-value-bind (a b c d e) (%mhv-up 4) (list a b c d e))
        (multiple-value-bind (a b) (%mhv-up 1) (list a b))
        (multiple-value-bind (a b) (%mhv-p1 0) (list a b))
        (nth-value 1 (%mhv-p1 2)) (nth-value 2 (%mhv-up 3)) (nth-value 4 (%mhv-up 5))
        (multiple-value-list (funcall #'%mhv-p1 2))
        (multiple-value-list (apply #'%mhv-up '(3)))
        (multiple-value-list (%mhv-p1-lit 1)) (multiple-value-list (%mhv-up-lit 1))
        (multiple-value-call #'list (%mhv-p1 2) (%mhv-up 3)))
  (:a :a nil nil (:a :b :c :d nil) (:a :b :c :d nil) (:a nil) (nil nil)
   :b :c :e (:a :b) (:a :b :c) (1 2 3) (1 2) (:a :b :a :b :c)))

;; Written where the values are consumed directly, inside one body.
(defun %mhv-local (n)
  (list (multiple-value-bind (a b c) (multiple-value-prog1 (%mhv-n n) (%mhv-noise)) (list a b c))
        (multiple-value-bind (a b c) (unwind-protect (%mhv-n n) (%mhv-noise)) (list a b c))
        (multiple-value-list (multiple-value-prog1 (%mhv-n n) (%mhv-noise)))
        (multiple-value-list (unwind-protect (%mhv-n n) (%mhv-noise)))
        (nth-value 1 (multiple-value-prog1 (%mhv-n n) (%mhv-noise)))
        (let ((x (multiple-value-prog1 (%mhv-n n) (%mhv-noise)))) x)
        (let ((x (unwind-protect (%mhv-n n) (%mhv-noise)))) x)))

(deftest mv-held-values.local
  (list (%mhv-local 0) (%mhv-local 1) (%mhv-local 3) (%mhv-local 5))
  (((nil nil nil) (nil nil nil) () () nil nil nil)
   ((:a nil nil) (:a nil nil) (:a) (:a) nil :a :a)
   ((:a :b :c) (:a :b :c) (:a :b :c) (:a :b :c) :b :a :a)
   ((:a :b :c) (:a :b :c) (:a :b :c :d :e) (:a :b :c :d :e) :b :a :a)))

;; Nested, and a value read after the form is not one the noise published.
(defun %mhv-nested (n)
  (multiple-value-prog1
      (unwind-protect (multiple-value-prog1 (%mhv-n n) (%mhv-noise))
        (%mhv-noise))
    (%mhv-noise)))
(defun %mhv-single-after (n)
  (let ((x (multiple-value-prog1 (%mhv-n n) (%mhv-noise))))
    (multiple-value-list x)))

(deftest mv-held-values.nested
  (list (multiple-value-list (%mhv-nested 2)) (multiple-value-list (%mhv-nested 4))
        (multiple-value-bind (a b c d) (%mhv-nested 4) (list a b c d)) (%mhv-nested 3)
        (%mhv-single-after 2) (%mhv-single-after 1))
  ((:a :b) (:a :b :c :d) (:a :b :c :d) :a (:a) (:a)))

;; Ways out of the held form that skip the hold: RETURN-FROM (to the function's
;; own block and to an inner one), THROW, and GO.
(defun %mhv-rf (k)
  (multiple-value-prog1
      (unwind-protect
           (progn (when (eq k :out) (return-from %mhv-rf (values 1 2 3)))
                  (%mhv-n 2))
        (%mhv-noise))
    (%mhv-noise)))
(defun %mhv-rf-inner (k)
  (block inner
    (multiple-value-prog1
        (progn (when k (return-from inner (values :x :y))) (%mhv-n 3))
      (%mhv-noise))))
(defun %mhv-throw ()
  (catch 'tag
    (multiple-value-prog1
        (unwind-protect (throw 'tag (values :t1 :t2)) (%mhv-noise))
      (%mhv-noise))))
(defun %mhv-go ()
  (let ((r nil))
    (tagbody
       (multiple-value-prog1 (go out) (setq r :not-here))
     out (setq r :out))
    (values r (multiple-value-list (%mhv-p1 2)))))

(deftest mv-held-values.exits
  (list (multiple-value-list (%mhv-rf :out)) (multiple-value-list (%mhv-rf :in))
        (multiple-value-bind (a b c) (%mhv-rf :out) (list a b c)) (%mhv-rf :out)
        (multiple-value-list (%mhv-rf-inner t)) (multiple-value-list (%mhv-rf-inner nil))
        (multiple-value-bind (a b) (%mhv-rf-inner t) (list a b))
        (multiple-value-list (%mhv-throw)) (multiple-value-list (%mhv-go)))
  ((1 2 3) (:a :b) (1 2 3) 1 (:x :y) (:a :b :c) (:x :y) (:t1 :t2) (:out (:a :b))))

;; The held form returns one value after something in it published values of
;; its own (READ-LINE's two): those are not the form's.
(defun %mhv-stale-up ()
  (unwind-protect (list (read-line (make-string-input-stream "a"))) (%mhv-noise)))
(defun %mhv-stale-p1 ()
  (multiple-value-prog1 (list (read-line (make-string-input-stream "a"))) (%mhv-noise)))
(deftest mv-held-values.stale-state
  (list (multiple-value-list (%mhv-stale-up)) (multiple-value-list (%mhv-stale-p1))
        (multiple-value-bind (a b) (%mhv-stale-up) (list a b))
        (multiple-value-bind (a b) (%mhv-stale-p1) (list a b))
        (multiple-value-list (unwind-protect (list (read-line (make-string-input-stream "a"))) nil)))
  ((("a")) (("a")) (("a") nil) (("a") nil) (("a"))))

;; A cleanup that exits non-locally replaces the values.
(defun %mhv-cleanup-exit ()
  (block b (unwind-protect (%mhv-n 3) (return-from b (values :c1 :c2)))))
(deftest mv-held-values.cleanup-exit
  (list (multiple-value-list (%mhv-cleanup-exit))
        (multiple-value-bind (a b c) (%mhv-cleanup-exit) (list a b c)))
  ((:c1 :c2) (:c1 :c2 nil)))

;; Each thread holds its own values.
(deftest mv-held-values.threads
  (let* ((ths (loop for i below 4
                    collect (let ((i i))
                              (dotcl:make-thread
                               (lambda ()
                                 (let ((ok t))
                                   (dotimes (k 20000 ok)
                                     (multiple-value-bind (a b c)
                                         (unwind-protect (values i k (* i k)) (%mhv-noise))
                                       (unless (and (eql a i) (eql b k) (eql c (* i k)))
                                         (setq ok nil)))))))))))
    (mapcar #'dotcl:thread-join ths))
  (t t t t))

(defun %mhv-noise2 () (values 9 8))
(defun %mhv-p1b (x) (multiple-value-prog1 (values x 2) (%mhv-noise2)))
(defun %mhv-upb (x) (unwind-protect (values x 2 3 4) (%mhv-noise2)))

(defun %mhv-alloc-loop (n)
  (declare (fixnum n))
  (let ((o (list 1)) (r nil))
    (dotimes (i n r)
      (multiple-value-bind (a b) (multiple-value-prog1 (values o r) (setq r nil)) (setq r (or b a)))
      (multiple-value-bind (a b c d) (unwind-protect (values o o o r) (setq r nil)) (setq r (or d c b a)))
      (multiple-value-bind (a b) (%mhv-p1b o) (setq r (or b a)))
      (multiple-value-bind (a b c d) (%mhv-upb o) (setq r (or d c b a)))
      (setq r (%mhv-upb o)))))

(deftest-compiled-only mv-held-values.allocates-nothing
  (< (bytes-per-op #'%mhv-alloc-loop) 1)
  t)
