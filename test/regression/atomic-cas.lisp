;;; Generic atomic operations on arbitrary CL places.
;;; dotcl:compare-and-swap / atomic-incf / atomic-decf expand through
;;; get-setf-expansion, so every SETF-able place works; a single global monitor
;;; makes them correct under concurrency (lock-based, not lock-free).

(require "dotcl-thread")

;; --- compare-and-swap on each place kind ---

(defvar *cas-special* 10)
(deftest cas-special-var-success
  (list (dotcl:compare-and-swap *cas-special* 10 20) *cas-special*)
  (t 20))

(deftest cas-special-var-failure
  (let ((*cas-special* 5))
    (list (dotcl:compare-and-swap *cas-special* 99 7) *cas-special*))
  (nil 5))

(deftest cas-car
  (let ((c (cons :a :b)))
    (list (dotcl:compare-and-swap (car c) :a :x) (car c)))
  (t :x))

(deftest cas-cdr-failure
  (let ((c (cons :a :b)))
    (list (dotcl:compare-and-swap (cdr c) :z :y) (cdr c)))
  (nil :b))

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
  (loop for old = *cas-stack*
        until (dotcl:compare-and-swap *cas-stack* old (cons x old))))
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
