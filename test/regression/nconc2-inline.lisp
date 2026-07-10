;;; 2-arg NCONC compiler inline (Runtime.Nconc2) — semantics must match the
;;; stdlib &rest defun exactly, on both the compiled call-site path and the
;;; funcall (Lisp function object) path.

(deftest nconc2.basic
  (list (nconc nil (list 1 2))
        (nconc (list 1 2) nil)
        (nconc (list 1) (list 2 3))
        (nconc nil nil))
  ((1 2) (1 2) (1 2 3) nil))

;; Destructive connection: result EQ first arg, tail EQ second arg.
(deftest nconc2.destructive-shares
  (let* ((a (list 1 2)) (b (list 3))
         (r (nconc a b)))
    (list (eq r a) (eq (cddr r) b) r))
  (t t (1 2 3)))

;; Dotted first arg: final cdr replaced (stdlib last+rplacd semantics).
(deftest nconc2.dotted-first
  (let* ((a (cons 1 2))
         (r (nconc a (list 3))))
    (list (eq r a) r))
  (t (1 3)))

;; Non-list first arg: same RPLACD type error on both paths.
(deftest nconc2.non-list-error
  (list (handler-case (progn (nconc 5 (list 1)) :no-error)
          (type-error () :type-error))
        (handler-case (progn (funcall (symbol-function 'nconc) 5 (list 1)) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error))

;; funcall path (stdlib &rest defun) equivalence.
(deftest nconc2.funcall-path
  (let ((f (symbol-function 'nconc)))
    (list (funcall f nil (list 1))
          (funcall f (list 1) (list 2))
          (let* ((a (list 1 2)) (b (list 3)) (r (funcall f a b)))
            (list (eq r a) (eq (cddr r) b)))))
  ((1) (1 2) (t t)))

;; APPEND 2-arg (existing inline, pinned here): first copied, second SHARED.
(deftest append2.share-second
  (let* ((a (list 1 2)) (b (list 3))
         (r (append a b)))
    (list (eq (cddr r) b) (not (eq r a)) r a))
  (t t (1 2 3) (1 2)))

(deftest append2.funcall-path
  (let* ((b (list 3))
         (r (funcall (symbol-function 'append) (list 1 2) b)))
    (list (eq (cddr r) b) r))
  (t (1 2 3)))
