;;; An INLINE function whose &REST list is only passed on to APPLY is
;;; substituted at the call site with that APPLY turned into a FUNCALL of the
;;; arguments, so the call conses no list. (APPLY f ... (LIST x ...)) is
;;; compiled as (FUNCALL f ... x ...).

(setf dotcl:*save-sil* t)

(declaim (inline %ira-call))
(defun %ira-call (function &rest args)
  "Apply FUNCTION to ARGS."
  (apply function args))

(declaim (inline %ira-call2))
(defun %ira-call2 (function first &rest args)
  (apply function first args))

(declaim (inline %ira-twice))
(defun %ira-twice (&rest args)
  (list args (apply #'list args)))

(defun %ira-use2 (f a b) (%ira-call f a b))
(defun %ira-use0 (f) (%ira-call f))
(defun %ira-use-first (a b c) (%ira-call2 #'list a b c))
(defun %ira-use-twice (a) (%ira-twice a))
(defun %ira-order ()
  (let ((log '()))
    (%ira-call (progn (push :f log) #'list) (progn (push :a log) 1) (progn (push :b log) 2))
    (nreverse log)))
(defun %ira-direct (f a b) (apply f a (list b a)))

(deftest inline-rest-apply.values
  (list (%ira-use2 #'list 1 2) (%ira-use2 #'+ 3 4) (%ira-use0 (lambda () :z))
        (%ira-use-first 1 2 3) (%ira-use-twice 5) (%ira-order)
        (%ira-direct #'list 1 2)
        (multiple-value-list (%ira-call #'floor 7 2)))
  ((1 2) 7 :z (1 2 3) ((5) (5)) (:f :a :b) (1 2 1) (3 1)))

(deftest-compiled-only inline-rest-apply.no-apply
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Apply" #'%ira-use2) (has "Apply" #'%ira-use-first)
          (has "Apply" #'%ira-direct) (has "%IRA-TWICE" #'%ira-use-twice)))
  (nil nil nil t))

(setf dotcl:*save-sil* nil)
