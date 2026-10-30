;;; Generic atomic operations on arbitrary CL places.
;;; dotcl:compare-and-swap / atomic-incf / atomic-decf expand through
;;; get-setf-expansion, so every SETF-able place works; a single global monitor
;;; makes them correct under concurrency (lock-based, not lock-free).  (CAR X) and
;;; (CDR X) are the exception: the runtime swaps the cons's field directly, with no
;;; lock (see the end of this file).
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
;; retry: no separate (racy) re-read (rationale: old-value beats a boolean flag).
(deftest cas-failure-returns-current-for-retry
  (let ((cell (list 100)))
    (let* ((prev (dotcl:compare-and-swap (car cell) 999 -1))   ; mismatch -> returns 100
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

;; --- one lock, whether or not dotcl-thread is loaded ---
;; dotcl-thread used to define these three macros again, with a lock of its own:
;; code expanded before (require "dotcl-thread") and code expanded after it then
;; took different locks for the same place, so neither excluded the other. Every
;; expansion that takes a lock now names the runtime's one lock; a cons field
;; takes none (the next section).

(defun %cas-lock-names (tree)
  (let ((names '()))
    (labels ((walk (x)
               (cond ((consp x) (walk (car x)) (walk (cdr x)))
                     ((and (symbolp x) (search "LOCK" (symbol-name x))
                           (boundp x))
                      (pushnew (symbol-name x) names :test #'string=)))))
      (walk tree))
    names))

(deftest cas-one-lock-for-every-expansion
  (mapcar (lambda (form) (%cas-lock-names (macroexpand-1 form)))
          '((dotcl:compare-and-swap (svref v 0) 1 2)
            (dotcl:atomic-incf (gethash k h))
            (dotcl:atomic-decf (svref v 0) 2)
            (dotcl:compare-and-swap *cas-special* 1 2)
            (dotcl:atomic-incf *cas-special*)
            (dotcl:compare-and-swap (cadr x) 1 2)))
  (("*CAS-LOCK*") ("*CAS-LOCK*") ("*CAS-LOCK*") ("*CAS-LOCK*") ("*CAS-LOCK*") ("*CAS-LOCK*")))

;; A globally special variable is read and written by the runtime under that
;; lock; the result is what the general expansion gives.
(defvar *cas-sv* 1)
(deftest cas-special-variable-values
  (list (dotcl:compare-and-swap *cas-sv* 1 2) *cas-sv*
        (dotcl:compare-and-swap *cas-sv* 1 3) *cas-sv*
        (dotcl:atomic-incf *cas-sv* 5) (dotcl:atomic-decf *cas-sv*)
        (let ((*cas-sv* 100))
          (list (dotcl:compare-and-swap *cas-sv* 100 101) *cas-sv*))
        *cas-sv*)
  (1 2 2 2 7 6 (100 101) 6))

;; --- cons fields: no lock ---
;; (CAR X), (CDR X), (FIRST X) and (REST X) expand to the runtime's %CAS-CAR /
;; %CAS-CDR (and %ATOMIC-DELTA-CAR / -CDR), which swap the field with
;; Interlocked.CompareExchange. The comparison is still EQL (the tests above run
;; through them), and they exclude one another on the same cell.

(deftest cas-cons-fields-take-no-lock
  (mapcar (lambda (form)
            (let ((expansion (macroexpand-1 form)))
              (list (symbol-name (car expansion)) (%cas-lock-names expansion))))
          '((dotcl:compare-and-swap (car x) 1 2)
            (dotcl:compare-and-swap (first x) 1 2)
            (dotcl:compare-and-swap (cdr x) 1 2)
            (dotcl:compare-and-swap (rest x) 1 2)
            (dotcl:atomic-incf (car x))
            (dotcl:atomic-decf (cdr x) 2)))
  (("%CAS-CAR" nil) ("%CAS-CAR" nil) ("%CAS-CDR" nil) ("%CAS-CDR" nil)
   ("%ATOMIC-DELTA-CAR" nil) ("%ATOMIC-DELTA-CDR" nil)))

;; The place's subform is evaluated once, before OLD and NEW.
(deftest cas-car-evaluation-order
  (let ((log '()) (cell (list 1)))
    (list (dotcl:compare-and-swap (car (progn (push :place log) cell))
                                  (progn (push :old log) 1)
                                  (progn (push :new log) 2))
          (car cell) (reverse log)))
  (1 2 (:place :old :new)))

(deftest cas-first-rest-and-incf-on-fields
  (let ((cell (list 10 20)))
    (list (dotcl:compare-and-swap (first cell) 10 11)
          (dotcl:compare-and-swap (rest cell) (cdr cell) '(21))
          (dotcl:atomic-incf (car cell) 4)
          (dotcl:atomic-decf (car cell))
          cell))
  (10 (20) 15 14 (14 21)))

(deftest cas-car-of-non-cons-is-a-type-error
  (list (handler-case (dotcl:compare-and-swap (car (eval 5)) 1 2)
          (type-error () :type-error))
        (handler-case (dotcl:atomic-incf (cdr (eval :k)))
          (type-error () :type-error)))
  (:type-error :type-error))

;; 8 threads x 5000 increments of one car: atomic-incf and a CAS retry loop at
;; the same time, so the two have to exclude each other. The count passes the
;; boxed-fixnum cache on the way, so the EQL comparison is exercised under
;; contention too.
(deftest cas-car-concurrent-mixed-increments
  (let ((cell (list 0)))
    (flet ((cas-incf ()
             (loop for old = (car cell)
                   until (eql (dotcl:compare-and-swap (car cell) old (1+ old)) old))))
      (let ((threads
              (loop for k below 8
                    collect (let ((k k))
                              (dotcl-thread:make-thread
                               (lambda ()
                                 (dotimes (i 5000)
                                   (if (evenp k) (dotcl:atomic-incf (car cell)) (cas-incf)))))))))
        (dolist (th threads) (dotcl-thread:thread-join th))))
    (car cell))
  40000)

;; A lock-free push onto a list held in a cdr; every push must land.
(deftest cas-cdr-concurrent-push
  (let ((head (list :head)))
    (let ((threads
            (loop repeat 8
                  collect (dotcl-thread:make-thread
                           (lambda ()
                             (dotimes (i 5000)
                               (loop for old = (cdr head)
                                     until (eq (dotcl:compare-and-swap (cdr head) old (cons i old))
                                               old))))))))
      (dolist (th threads) (dotcl-thread:thread-join th)))
    (length (cdr head)))
  40000)
