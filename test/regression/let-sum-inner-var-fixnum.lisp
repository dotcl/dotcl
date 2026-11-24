;;; Values come from SBCL. An inner LET variable bound to the sum of a fixnum and a small number,
;;; used only in comparisons and in assignments to a FIXNUM variable (babel's UTF-8 octet counter),
;;; gives the same values and the same errors whether or not the compiler keeps it unboxed:
;;; a sum past MOST-POSITIVE-FIXNUM is a bignum that the assignment to the fixnum variable rejects
;;; with a TYPE-ERROR, and a variable that is only compared keeps the bignum value.

(defun %lsi-count (s max)
  (declare (type simple-string s) (fixnum max))
  (loop with n fixnum = 0
        for i fixnum from 0 below (length s)
        for code = (char-code (schar s i)) do
        (let ((new (+ (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)) n)))
          (if (and (plusp max) (> new max))
              (loop-finish)
              (setq n new)))
        finally (return (values n i))))

(deftest let-sum-inner-var-fixnum.counter
  (list (multiple-value-list (%lsi-count "abc" 0))
        (multiple-value-list (%lsi-count (coerce (list #\a (code-char 233) (code-char 12354) #\b) 'string) 0))
        (multiple-value-list (%lsi-count (coerce (list #\a (code-char 233) (code-char 12354) #\b) 'string) 4))
        (multiple-value-list (%lsi-count "" 0)))
  ((3 3) (7 4) (3 2) (0 0)))

(defun %lsi-assign (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (setq n new))
  n)

(defun %lsi-compare-only (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (> new n)))

(defun %lsi-compare-cond (n m)
  (declare (fixnum n m))
  (let ((new (+ n (cond ((< m 0) 1) (t 4)))))
    (if (> new m) :big :small)))

(defun %lsi-compare-range (n m)
  (declare (fixnum n m))
  (let ((new (+ n m)))
    (and (> new 0) (< new 5))))

(defun %lsi-compare-guarded (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (if (and (plusp n) (> new n)) :a :b)))

(defun %lsi-escapes (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (lambda () new)))

(defun %lsi-passed (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (list new)))

(defun %lsi-shadow (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (let ((new (+ new 1)))
      (setq n new))
    n))

(defun %lsi-rebound (n)
  (declare (fixnum n))
  (let ((new (+ n 1)))
    (setq new (* new 2))
    (setq n new)
    n))

(defun %lsi-type-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (type-error () t)))

(deftest let-sum-inner-var-fixnum.boundary
  (list (%lsi-assign 5)
        (%lsi-assign (1- most-positive-fixnum))
        (%lsi-compare-only most-positive-fixnum)
        (%lsi-compare-cond most-positive-fixnum 5)
        (%lsi-compare-range most-positive-fixnum 5)
        (%lsi-compare-guarded most-positive-fixnum)
        (funcall (%lsi-escapes most-positive-fixnum))
        (%lsi-passed most-positive-fixnum)
        (%lsi-shadow 5)
        (%lsi-rebound 5))
  (6 #.(1- (1+ most-positive-fixnum)) t :big nil :a #.(1+ most-positive-fixnum)
   (#.(1+ most-positive-fixnum)) 7 12))

;;; The errors for a sum past the fixnum range come from the declared type of the variable the
;;; sum is stored into, which a build without the emitter (emit-free) does not check.
(deftest-emitting-only let-sum-inner-var-fixnum.boundary-errors
  (list (%lsi-type-error-p (lambda () (%lsi-assign most-positive-fixnum)))
        (%lsi-type-error-p (lambda () (%lsi-shadow (1- most-positive-fixnum))))
        (%lsi-type-error-p (lambda () (%lsi-rebound (floor most-positive-fixnum 2)))))
  (t t t))
