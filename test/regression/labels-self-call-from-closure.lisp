;;; A LABELS function that calls itself from a closure built inside its own body.
;;;
;;; The call must reach the LABELS binding. It used to reach a global function
;;; of that name instead, which normally does not exist, so the shape below
;;; failed with "Undefined function". Only the innermost level was wrong: the
;;; same call written directly in the LABELS body, or in a closure built in the
;;; LABELS *body* rather than in the function's own body, always worked.
;;;
;;; The shape is ordinary -- a recursive local function that recurs over a list
;;; through SOME / EVERY / MAPCAR is how a graph walk is usually written.

;;; Tail position inside the closure, one closure level.

(defun lscc-some (c)
  (labels ((try (n) (if (> n 10) n (some (lambda (x) (try (+ n x))) '(3)))))
    (try c)))

(defun lscc-every (c)
  (labels ((try (n) (if (> n 10) n (every (lambda (x) (try (+ n x))) '(3)))))
    (try c)))

(defun lscc-mapcar (c)
  (labels ((try (n) (if (> n 10) (list n) (mapcar (lambda (x) (try (+ n x))) '(3)))))
    (try c)))

(deftest labels-self-call-from-closure-some
  (lscc-some 1)
  13)

(deftest labels-self-call-from-closure-every
  (lscc-every 1)
  t)

(deftest labels-self-call-from-closure-mapcar
  (lscc-mapcar 1)
  (((((13))))))

;;; Non-tail position inside the closure: the recursive value is an operand.

(defun lscc-nontail (c)
  (labels ((try (n)
             (if (> n 10)
                 n
                 (+ 1 (reduce #'+ (mapcar (lambda (x) (try (+ n x))) '(3)))))))
    (try c)))

(deftest labels-self-call-from-closure-nontail
  (lscc-nontail 1)
  17)

;;; Two closure levels: the self-call is inside a lambda inside a lambda.

(defun lscc-nested (c)
  (labels ((try (n)
             (if (> n 10)
                 n
                 (some (lambda (r) (some (lambda (x) (try (+ n x))) r)) '((3))))))
    (try c)))

(deftest labels-self-call-from-closure-nested
  (lscc-nested 1)
  13)

;;; The closure may also name the function as a value.

(defun lscc-sharp-quote (c)
  (labels ((try (n) (if (> n 10) n (some (lambda (x) (funcall #'try (+ n x))) '(3)))))
    (try c)))

(deftest labels-self-call-from-closure-sharp-quote
  (lscc-sharp-quote 1)
  13)

;;; Two LABELS functions, mutually recursive through closures.

(defun lscc-mutual (c)
  (labels ((up (n) (if (> n 10) n (some (lambda (x) (down (+ n x))) '(3))))
           (down (n) (if (> n 10) n (some (lambda (x) (up (+ n x))) '(2)))))
    (up c)))

(deftest labels-self-call-from-closure-mutual
  (lscc-mutual 1)
  11)

;;; A variable of the same name must not be confused with the function cell:
;;; Common Lisp is a Lisp-2 and the closure captures both.

(defun lscc-lisp-2 (c)
  (let ((try 100))
    (labels ((try (n) (if (> n 10) (+ n try) (some (lambda (x) (try (+ n x))) '(3)))))
      (try c))))

(deftest labels-self-call-from-closure-lisp-2
  (lscc-lisp-2 1)
  113)

;;; What the fix must not cost: a LABELS function whose self-calls are all
;;; direct tail calls still recurs without growing the stack, so a depth that
;;; would overflow a frame-per-call implementation has to return a value.

(defun lscc-tail-loop (n)
  (labels ((down (k acc) (if (<= k 0) acc (down (- k 1) (+ acc 1)))))
    (down n 0)))

(deftest labels-self-call-from-closure-tail-still-looped
  (lscc-tail-loop 300000)
  300000)
