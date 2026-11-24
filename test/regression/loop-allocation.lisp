;;; DOLIST, DOTIMES, DO, DO* and LOOP accumulators allocate only the result they
;;; return. The allocation assertions are compiled-only: the tree-walk
;;; interpreter allocates on every iteration by design, so they do not hold there.

(defparameter *la-it* 1000)
(defparameter *la-n* 1000)
(defparameter *la-lst* (loop for i below *la-n* collect i))
(defparameter *la-vec* (coerce *la-lst* 'vector))
(defparameter *la-dbl* (loop for i below *la-n* collect (coerce i 'double-float)))
(defparameter *la-lol* (loop for i below *la-n* collect (list i)))
(defparameter *la-sink* nil)

(defun %la-baseline ()
  (/ (min-allocated-bytes (lambda () (do ((k 0 (1+ k))) ((= k *la-it*)) (setq *la-sink* 1))) 5)
     (float *la-it*)))

(defun %la-size (kind)
  (let ((base (%la-baseline)))
    (- (/ (min-allocated-bytes
           (lambda ()
             (do ((k 0 (1+ k))) ((= k *la-it*))
               (setq *la-sink* (ecase kind
                                 (:cons (cons k k))
                                 (:fixnum (+ 499500 k))
                                 (:double (float k 1d0))))))
           5)
          (float *la-it*))
       base)))

(defun %la-floor (kind)
  (case kind
    (:zero 0)
    (:fixnum (%la-size :fixnum))
    (:double (%la-size :double))
    (:list (* *la-n* (%la-size :cons)))))

(defun %la-bad (rows)
  (let ((base (%la-baseline)) (bad nil))
    (dolist (row rows (nreverse bad))
      (destructuring-bind (label kind thunk) row
        (funcall thunk)
        (let ((got (min-allocated-bytes thunk 5))
              (floor (* *la-it* (+ (%la-floor kind) base))))
          (when (> got floor) (push (list label got floor) bad)))))))

(defmacro %la-row (label kind form)
  `(list ,label ,kind (lambda () (do ((k 0 (1+ k))) ((= k *la-it*)) (setq *la-sink* ,form)))))

(deftest loop-allocation.collect-order
  (list (loop for x in '(1 2 3) collect x)
        (loop for x in '(1 2 3) collect x into r finally (return r))
        (loop for x in '(1 2 3) append (list x x))
        (loop for x in '(1 2 3) nconc (list x))
        (loop for x in '(1 2 3) collect x into r
              collect (* x x) into s finally (return (list r s)))
        (loop for x in nil collect x)
        (loop for x in '(nil (1 2) nil (3 4)) append x)
        (loop for x in '(nil (1 2) nil (3 4)) nconc (copy-list x)))
  ((1 2 3) (1 2 3) (1 1 2 2 3 3) (1 2 3) ((1 2 3) (1 4 9)) nil (1 2 3 4) (1 2 3 4)))

(deftest loop-allocation.nconc-shares-head
  (let ((a (list 1 2)) (b (list 3 4)))
    (eq a (loop for x in (list a b) nconc x)))
  t)

(deftest-compiled-only loop-allocation.accumulators
  (%la-bad
   (list
    (%la-row "dotimes-incf" :fixnum
      (let ((s 0)) (declare (fixnum s)) (dotimes (i *la-n* s) (declare (fixnum i)) (incf s i))))
    (%la-row "dolist-incf" :fixnum
      (let ((s 0)) (declare (fixnum s)) (dolist (x *la-lst* s) (declare (fixnum x)) (incf s x))))
    (%la-row "dolist-return" :fixnum
      (let ((s 0)) (declare (fixnum s)) (dolist (x *la-lst* s) (declare (fixnum x))
                                         (when (= x 999) (return s)) (incf s x))))
    (%la-row "do-two" :fixnum
      (do ((i 0 (1+ i)) (j *la-n* (1- j)) (s 0)) ((>= i j) s) (declare (fixnum i j s)) (incf s i)))
    (%la-row "do*-two" :fixnum
      (do* ((i 0 (1+ i)) (j *la-n* (1- j)) (s 0)) ((>= i j) s) (declare (fixnum i j s)) (incf s i)))
    (%la-row "loop-below" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for i of-type fixnum from 0 below *la-n* do (incf s i)) s))
    (%la-row "loop-downto" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for i of-type fixnum from *la-n* downto 0 do (incf s i)) s))
    (%la-row "loop-by2" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for i of-type fixnum from 0 below *la-n* by 2 do (incf s i)) s))
    (%la-row "loop-in" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for x of-type fixnum in *la-lst* do (incf s x)) s))
    (%la-row "loop-across" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for x of-type fixnum across *la-vec* do (incf s x)) s))
    (%la-row "loop-being" :fixnum
      (let ((s 0)) (declare (fixnum s))
        (loop for x of-type fixnum being the elements of *la-lst* do (incf s x)) s))
    (%la-row "loop-equals-then" :zero
      (let ((s 0)) (loop for x = 1 then 2 repeat *la-n* do (incf s x)) s))
    (%la-row "loop-equals-then-declared" :zero
      (let ((s 0)) (declare (fixnum s))
        (loop for x of-type fixnum = 1 then 2 repeat *la-n* do (incf s x)) s))
    (%la-row "loop-repeat" :zero
      (let ((s 0)) (loop repeat *la-n* do (incf s 1)) s))
    (%la-row "loop-repeat-declared" :zero
      (let ((s 0)) (declare (fixnum s)) (loop repeat *la-n* do (incf s 1)) s))
    (%la-row "sum-fix" :fixnum
      (loop for x of-type fixnum in *la-lst* sum x into s of-type fixnum finally (return s)))
    (%la-row "sum-double" :double
      (loop for x of-type double-float in *la-dbl* sum x into s of-type double-float
            finally (return s)))
    (%la-row "count-fix" :zero
      (loop for x in *la-lst* count (evenp x)))
    (%la-row "count-fix-declared" :zero
      (loop for x of-type fixnum in *la-lst* count (evenp x) into c of-type fixnum
            finally (return c)))
    (%la-row "maximize-fix" :zero
      (loop for x in *la-lst* maximize x))
    (%la-row "maximize-fix-declared" :zero
      (loop for x of-type fixnum in *la-lst* maximize x into m of-type fixnum finally (return m)))
    (%la-row "minimize-fix" :zero
      (loop for x in *la-lst* minimize x))
    (%la-row "minimize-fix-declared" :zero
      (loop for x of-type fixnum in *la-lst* minimize x into m of-type fixnum finally (return m)))
    (%la-row "maximize-double" :zero
      (loop for x in *la-dbl* maximize x))
    (%la-row "maximize-double-declared" :double
      (loop for x of-type double-float in *la-dbl* maximize x into m of-type double-float
            finally (return m)))
    (%la-row "minimize-double" :zero
      (loop for x in *la-dbl* minimize x))
    (%la-row "minimize-double-declared" :double
      (loop for x of-type double-float in *la-dbl* minimize x into m of-type double-float
            finally (return m)))
    (%la-row "collect-default" :list
      (loop for x in *la-lst* collect x))
    (%la-row "collect-default-declared" :list
      (loop for x of-type fixnum in *la-lst* collect x))
    (%la-row "collect-into" :list
      (loop for x in *la-lst* collect x into r finally (return r)))
    (%la-row "collect-into-declared" :list
      (loop for x of-type fixnum in *la-lst* collect x into r of-type list finally (return r)))
    (%la-row "append" :list
      (loop for x in *la-lol* append x))
    (%la-row "append-declared" :list
      (loop for x of-type list in *la-lol* append x into r of-type list finally (return r)))
    (%la-row "nconc" :list
      (loop for x in *la-lol* nconc (copy-list x)))
    (%la-row "nconc-declared" :list
      (loop for x of-type list in *la-lol* nconc (copy-list x) into r of-type list
            finally (return r)))
    (%la-row "loop-while" :fixnum
      (loop for x of-type fixnum in *la-lst* while (< x 999) sum x into s of-type fixnum
            finally (return s)))
    (%la-row "loop-until" :fixnum
      (loop for x of-type fixnum in *la-lst* until (= x 999) sum x into s of-type fixnum
            finally (return s)))
    (%la-row "loop-always" :zero
      (loop for x in *la-lst* always (>= x 0)))
    (%la-row "loop-always-declared" :zero
      (loop for x of-type fixnum in *la-lst* always (>= x 0)))
    (%la-row "loop-never" :zero
      (loop for x in *la-lst* never (< x 0)))
    (%la-row "loop-never-declared" :zero
      (loop for x of-type fixnum in *la-lst* never (< x 0)))
    (%la-row "loop-thereis" :zero
      (loop for x in *la-lst* thereis (= x 999)))
    (%la-row "loop-thereis-declared" :zero
      (loop for x of-type fixnum in *la-lst* thereis (= x 999)))
    (%la-row "loop-when-sum" :fixnum
      (loop for x of-type fixnum in *la-lst* when (evenp x) sum x into s of-type fixnum
            finally (return s)))
    (%la-row "loop-values" :fixnum
      (multiple-value-bind (a b)
          (loop for x of-type fixnum in *la-lst* count (evenp x) into c of-type fixnum
                sum x into s of-type fixnum finally (return (values s c)))
        (setq *la-sink* (if (= a b) a b))))))
  nil)
