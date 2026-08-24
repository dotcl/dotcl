;;; Generic atomic operations on arbitrary CL places.
;;; dotcl:compare-and-swap / atomic-incf / atomic-decf expand through
;;; get-setf-expansion, so every SETF-able place works; a single global monitor
;;; makes them correct under concurrency (lock-based, not lock-free).
;;; compare-and-swap returns the PRIOR value (sb-ext/CCL convention): success is
;;; (eq old ret), and a failed CAS returns the current value for the next retry.

(require "dotcl-thread")

;; --- compare-and-swap on each place kind ---

(defvar *cas-special* 10)
(deftest cas-special-var-success        ; matches: stores 20, returns the prior value 10
  (list (dotcl:compare-and-swap *cas-special* 10 20) *cas-special*)
  (10 20))

(deftest cas-special-var-failure        ; no match: returns the current value 5, unchanged
  (let ((*cas-special* 5))
    (list (dotcl:compare-and-swap *cas-special* 99 7) *cas-special*))
  (5 5))

(deftest cas-car
  (let ((c (cons :a :b)))
    (list (dotcl:compare-and-swap (car c) :a :x) (car c)))
  (:a :x))

(deftest cas-cdr-failure
  (let ((c (cons :a :b)))
    (list (dotcl:compare-and-swap (cdr c) :z :y) (cdr c)))
  (:b :b))

(deftest cas-svref
  (let ((v (vector 1 2 3)))
    (dotcl:compare-and-swap (svref v 1) 2 99)
    (svref v 1))
  99)

(deftest cas-gethash
  (let ((h (make-hash-table)))
    (setf (gethash :k h) 5)
    (dotcl:compare-and-swap (gethash :k h) 5 6)
    (nth-value 0 (gethash :k h)))
  6)

(defclass %cas-pt () ((x :initform 0)))
(deftest cas-slot-value
  (let ((p (make-instance '%cas-pt)))
    (dotcl:compare-and-swap (slot-value p 'x) 0 7)
    (slot-value p 'x))
  7)

(defstruct %cas-box val)
(deftest cas-struct-slot
  (let ((b (make-%cas-box :val 1)))
    (dotcl:compare-and-swap (%cas-box-val b) 1 2)
    (%cas-box-val b))
  2)

;; The failing return IS the current cell value, usable directly as the next OLD in a
;; retry — no separate (racy) re-read (rationale: old-value beats a boolean flag).
(deftest cas-failure-returns-current-for-retry
  (let ((cell (list 100)))
    (let* ((prev (dotcl:compare-and-swap (car cell) 999 -1))   ; mismatch → returns 100
           (ok   (dotcl:compare-and-swap (car cell) prev 200))) ; retry with prev succeeds
      (list prev ok (car cell))))
  (100 100 200))

;; --- atomic-incf / atomic-decf return the NEW value ---

(deftest atomic-incf-decf-return-new
  (let ((n 0))
    (list (dotcl:atomic-incf n 5)   ; 5
          (dotcl:atomic-incf n)     ; 6
          (dotcl:atomic-decf n 2)   ; 4
          (dotcl:atomic-decf n)     ; 3
          n))
  (5 6 4 3 3))

;; --- concurrency: no lost updates ---
;; 8 threads x 5000 atomic-incf on one shared counter must total exactly 40000.
(defvar *cas-shared* 0)
(deftest atomic-incf-no-lost-updates
  (progn
    (setf *cas-shared* 0)
    (let ((threads
            (loop repeat 8
                  collect (dotcl-thread:make-thread
                           (lambda () (dotimes (i 5000) (dotcl:atomic-incf *cas-shared*)))))))
      (dolist (th threads) (dotcl-thread:thread-join th)))
    *cas-shared*)
  40000)

;; CAS retry loop implementing a lock-free concurrent push; every push must land.
(defvar *cas-stack* nil)
(defun %cas-push (x)
  ;; Old-value CAS: success is (eq ret old). The failing return is the fresh value,
  ;; but this loop re-reads *cas-stack* each turn anyway, so it just retries.
  (loop for old = *cas-stack*
        until (eq (dotcl:compare-and-swap *cas-stack* old (cons x old)) old)))
(deftest compare-and-swap-lock-free-push
  (progn
    (setf *cas-stack* nil)
    (let ((threads
            (loop repeat 8
                  collect (dotcl-thread:make-thread
                           (lambda () (dotimes (i 5000) (%cas-push 1)))))))
      (dolist (th threads) (dotcl-thread:thread-join th)))
    (length *cas-stack*))
  40000)

;; --- the comparison is EQL, not EQ ---
;; Fixnums are boxed here and only a small range is cached, so an EQ comparison
;; made a CAS on a counter stop swapping once the value left that cache. The old
;; value is computed separately from the stored one in each test, so it is a
;; distinct object that is merely EQL.

(deftest cas-large-fixnum-distinct-object
  (let* ((stored (* 1000 1000))          ; 1000000, outside the small-int cache
         (probe  (* 1000 1000))
         (cell   (list stored)))
    (list (eq stored probe)              ; distinct objects...
          (eql stored probe)             ; ...that are EQL
          (dotcl:compare-and-swap (car cell) probe 0)
          (car cell)))
  (nil t 1000000 0))

(deftest cas-bignum-distinct-object
  (let* ((stored (* 1000000 1000000))
         (probe  (* 1000000 1000000))
         (cell   (list stored)))
    (list (dotcl:compare-and-swap (car cell) probe :swapped) (car cell)))
  (1000000000000 :swapped))

(deftest cas-float-and-char-by-value
  (let ((f (list (+ 0.5d0 0.25d0)))
        (c (list (code-char 955))))
    (list (dotcl:compare-and-swap (car f) 0.75d0 :f) (car f)
          (char= (dotcl:compare-and-swap (car c) (code-char 955) :c) (code-char 955))
          (car c)))
  (0.75d0 :f t :c))

;; A counter crossing the cached range must keep swapping the whole way.
(deftest cas-counter-across-cache-boundary
  (let ((cell (list 65500)))
    (loop repeat 100
          for cur = (car cell)
          do (dotcl:compare-and-swap (car cell) cur (1+ cur)))
    (car cell))
  65600)

;; EQL is not EQUAL: two structurally equal conses are still different places,
;; so a CAS against a copy must fail.
(deftest cas-cons-stays-identity-compared
  (let* ((stored (list 1 2))
         (cell   (list stored)))
    (list (dotcl:compare-and-swap (car cell) (list 1 2) :swapped)
          (eq (car cell) stored)))
  ((1 2) t))
