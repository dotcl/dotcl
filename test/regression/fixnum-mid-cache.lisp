;;; Integers just above the eager small-integer cache are boxed once and shared; the values around
;;; the edges of the cache ranges keep their identity under EQL and their arithmetic.

(require "dotcl-thread")

(deftest fixnum-mid-cache.edges
  (let ((edges '(-129 -128 -127 -1 0 1 65534 65535 65536 65537 262142 262143 262144 262145 1000000)))
    (mapcar (lambda (n) (list n (eql (+ n 0) n) (= (- (+ n 1) 1) n) (integerp (1+ n)))) edges))
  ((-129 t t t) (-128 t t t) (-127 t t t) (-1 t t t) (0 t t t) (1 t t t) (65534 t t t) (65535 t t t)
   (65536 t t t) (65537 t t t) (262142 t t t) (262143 t t t) (262144 t t t) (262145 t t t)
   (1000000 t t t)))

(deftest fixnum-mid-cache.list-of-counters
  (let ((acc nil))
    (dotimes (j 300000) (push j acc))
    (list (length acc) (car acc) (nth 100000 acc) (nth 299999 acc) (apply #'+ (subseq acc 0 5))))
  (300000 299999 199999 0 1499985))

(deftest fixnum-mid-cache.same-value-same-object
  (let ((a (+ 100000 (random 1))) (b (+ 99999 (random 1) 1)))
    (list (eql a b) (eq a a) (= a b) (equal (list a) (list b))))
  (t t t t))

(deftest fixnum-mid-cache.mutating-a-list-does-not-share
  (let ((l (list 100000 100000)))
    (setf (car l) 200000)
    l)
  (200000 100000))

(deftest fixnum-mid-cache.threads-fill-the-same-slots
  (let* ((threads (loop repeat 4
                        collect (dotcl-thread:make-thread
                                 (lambda ()
                                   (let ((bad 0) (acc nil))
                                     (dotimes (j 200000)
                                       (let ((n (+ j 70000)))
                                         (unless (and (eql n (+ 70000 j)) (= n (- (+ n 1) 1))) (incf bad))
                                         (when (zerop (mod j 50000)) (push n acc))))
                                     (list bad (length acc)))))))
         (results (mapcar #'dotcl-thread:thread-join threads)))
    results)
  ((0 4) (0 4) (0 4) (0 4)))
