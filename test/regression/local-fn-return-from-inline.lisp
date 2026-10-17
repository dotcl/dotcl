;;; A local function that RETURN-FROMs a named block outside itself is
;;; substituted at its call sites without an INLINE declaration, so the escape
;;; is a branch instead of a .NET exception. The answers must not depend on
;;; whether a call site took the substitution.

(setf dotcl:*save-sil* t)

(defun %lfri-token (chars)
  (block read-token
    (let ((acc '()))
      (labels ((push-char (c) (push c acc))
               (terminate () (return-from read-token (coerce (nreverse acc) 'string))))
        (flet ((next ()
                 (if chars (pop chars) (terminate))))
          (loop (let ((c (next)))
                  (if (char= c #\Space)
                      (terminate)
                      (push-char c)))))))))

(deftest local-fn-return-from-inline.token
  (list (%lfri-token (coerce "abc def" 'list))
        (%lfri-token (coerce "xyz" 'list)))
  ("abc" "xyz"))

(deftest-compiled-only local-fn-return-from-inline.no-exception
  (search "BlockReturnException" (%flat-text (dotcl:function-sil #'%lfri-token)))
  nil)

(deftest local-fn-return-from-inline.values
  (multiple-value-list
   (block b
     (flet ((f (x) (when x (return-from b (values 1 2 3))) 0))
       (f nil)
       (f t)
       :not-reached)))
  (1 2 3))

;;; The call site rebinds the block name: the escape still goes to the block the
;;; definition saw.
(deftest local-fn-return-from-inline.shadowed-block
  (block b
    (flet ((f () (return-from b :outer)))
      (block b (f))
      :inner-fell-through))
  :outer)

(deftest local-fn-return-from-inline.notinline
  (block b
    (flet ((f () (return-from b :ok)))
      (declare (notinline f))
      (f)
      :no))
  :ok)

(deftest local-fn-return-from-inline.cleanup
  (let ((log '()))
    (list (block b
            (flet ((f () (unwind-protect (return-from b :ret) (push :cleanup log))))
              (f)
              :no))
          log))
  (:ret (:cleanup)))

(defvar *lfri-special* :global)

(deftest local-fn-return-from-inline.special-unbound-on-exit
  (list (block b
          (flet ((f () (let ((*lfri-special* :bound)) (return-from b *lfri-special*))))
            (f)))
        *lfri-special*)
  (:bound :global))

(deftest local-fn-return-from-inline.used-as-value
  (block b
    (flet ((f (x) (when (> x 1) (return-from b x)) x))
      (mapc #'f '(0 1 2 3))
      :no))
  2)

(deftest local-fn-return-from-inline.called-from-closure
  (block b
    (flet ((f (x) (when (> x 1) (return-from b (* x 10)))))
      (mapc (lambda (x) (f x)) '(0 1 2 3))
      :no))
  20)

(deftest local-fn-return-from-inline.argument-order
  (let ((log '()))
    (list (block b
            (flet ((f (x y) (return-from b (list x y))))
              (f (progn (push 1 log) :a) (progn (push 2 log) :b))))
          (reverse log)))
  ((:a :b) (1 2)))
