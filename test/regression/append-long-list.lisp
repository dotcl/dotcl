;;; APPEND must not recurse once per element.
;;;
;;; It used to cons the copy of its first argument on the way out of a
;;; per-element recursion, so a long enough list exhausted the stack. A .NET
;;; StackOverflowException cannot be caught, so this killed the process outright
;;; rather than signalling — and the compiler builds instruction lists with
;;; APPEND, so a large (but perfectly legal) source form took the compiler with
;;; it. Three million elements was past the edge; two million was not.

(defun %append-test-list (n)
  (let ((l nil))
    (dotimes (i n) (push i l))
    l))

(deftest append-three-million-elements
  (let ((r (append (%append-test-list 3000000) '(:tail))))
    (list (length r) (first r) (car (last r))))
  (3000001 2999999 :tail))

;;; Sharing and copying: the first argument is copied, the second is shared.
(deftest append-copies-first-shares-second
  (let* ((a (list 1 2 3))
         (b (list 4 5))
         (r (append a b)))
    (list r (eq (cdddr r) b) (eq r a)))
  ((1 2 3 4 5) t nil))

(deftest append-nil-first-returns-second
  (let ((b (list 1 2)))
    (eq (append nil b) b))
  t)

;;; A dotted first argument is a type error, not a silent truncation.
(deftest append-dotted-first-signals
  (handler-case (progn (append (list* 1 2 3) '(4)) :no-error)
    (type-error () :type-error))
  :type-error)
