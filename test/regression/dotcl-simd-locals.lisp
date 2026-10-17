;;; Locals declared a DOTCL-SIMD pack type, held as raw Vector128 values,
;;; against the same code with the operations NOTINLINE (boxed locals).
;;; Uses the helpers of dotcl-simd-ops.lisp and dotcl-simd-lower.lisp.

(require "dotcl-simd")

(defun %dsloc-count (needle text)
  (loop with start = 0
        for pos = (search needle text :start2 start)
        while pos count t do (setf start (1+ pos))))

(defun %dsloc-same-p (a b)
  (if (and (consp a) (consp b))
      (and (= (length a) (length b)) (every #'%dsloc-same-p a b))
      (%dsl-same-p a b)))

(defun %dsloc-differences (name make-body)
  (let* ((type (simd-sym name))
         (add (simd-sym name "+"))
         (xor (simd-sym name "-XOR"))
         (state (make-random-state t))
         (bad '()))
    (multiple-value-bind (lowered boxed)
        (%dsl-compile-pair '(a b c) (lambda () (funcall make-body type add xor)) (list add xor))
      (dotimes (i 20)
        (let* ((args (loop repeat 3 collect (simd-random-pack name state)))
               (l (%dsl-run lowered args)) (b (%dsl-run boxed args)))
          (unless (%dsloc-same-p l b) (push (list name args l b) bad)))))
    (subseq bad 0 (min 2 (length bad)))))

(deftest-emitting-only dotcl-simd-locals.let-accumulator
  (loop for name in *simd-names*
        append (%dsloc-differences
                name
                (lambda (type add xor)
                  `(let ((acc (,add a b)) (k (,xor a c)))
                     (declare (type ,type acc k))
                     (dotimes (i 3) (setq acc (,add acc k)))
                     (list acc (setq k (,xor k acc)) k)))))
  nil)

(deftest-emitting-only dotcl-simd-locals.let*-and-do
  (loop for name in *simd-names*
        append (%dsloc-differences
                name
                (lambda (type add xor)
                  `(let* ((acc (,add a b)) (k (,xor acc c)))
                     (declare (type ,type acc k))
                     (do ((j 0 (1+ j)) (m (,add k k) (,xor m acc)))
                         ((= j 4) (list acc k m))
                       (declare (type ,type m))
                       (setf acc (,add acc m)
                             k (,xor k (,add acc c))))))))
  nil)

(deftest-emitting-only dotcl-simd-locals.escapes
  ;; Read as an object: a call argument, multiple values, an inner binding.
  (loop for name in *simd-names*
        append (%dsloc-differences
                name
                (lambda (type add xor)
                  (declare (ignore xor))
                  `(let ((acc (,add a b)))
                     (declare (type ,type acc))
                     (list (multiple-value-list (values acc 1))
                           (identity acc)
                           (let ((acc 5)) acc)
                           (let ((x acc)) (setq acc (,add acc c)) x)
                           acc)))))
  nil)

(deftest-emitting-only dotcl-simd-locals.closure-capture
  (let ((f (compile nil '(lambda (a b)
                          (let ((acc (dotcl-simd:f64.2+ a b)))
                            (declare (type dotcl-simd:f64.2 acc))
                            (let ((get (lambda () acc))
                                  (bump (lambda () (setq acc (dotcl-simd:f64.2+ acc b)))))
                              (setq acc (dotcl-simd:f64.2+ acc a))
                              (funcall bump)
                              (list (multiple-value-list (dotcl-simd:f64.2-values (funcall get)))
                                    (multiple-value-list (dotcl-simd:f64.2-values acc)))))))))
    (funcall f (dotcl-simd:make-f64.2 1d0 2d0) (dotcl-simd:make-f64.2 10d0 20d0)))
  ((22d0 44d0) (22d0 44d0)))

(deftest-emitting-only dotcl-simd-locals.setq-wrong-type
  (let ((f (compile nil '(lambda (a v)
                          (let ((acc (dotcl-simd:s32.4+ a a)))
                            (declare (type dotcl-simd:s32.4 acc))
                            (setq acc v)
                            acc)))))
    (flet ((try (v)
             (handler-case (progn (funcall f (dotcl-simd:make-s32.4 1 2 3 4) v) :no-error)
               (type-error (e) (list (eq (type-error-datum e) v)
                                     (type-error-expected-type e))))))
      (list (try 3)
            (try (dotcl-simd:make-u32.4 1 2 3 4))
            (multiple-value-list
             (dotcl-simd:s32.4-values (funcall f (dotcl-simd:make-s32.4 1 2 3 4)
                                               (dotcl-simd:make-s32.4 5 6 7 8)))))))
  ((t dotcl-simd:s32.4) (t dotcl-simd:s32.4) (5 6 7 8)))

(deftest-compiled-only dotcl-simd-locals.loop-shape
  ;; The accumulator stays raw, also into the horizontal sum: no box.
  (let ((dotcl:*save-sil* t))
    (eval '(defun %dsloc-dot (x y n)
            (declare (type (simple-array double-float (*)) x y)
                     (type (integer 0 1000) n)
                     (optimize (speed 3) (safety 0)))
            (let ((acc (dotcl-simd:f64.2 0d0)))
              (declare (type dotcl-simd:f64.2 acc))
              (do ((i 0 (+ i 2))) ((>= i n))
                (declare (type (integer 0 1002) i))
                (setf acc (dotcl-simd:f64.2+ acc (dotcl-simd:f64.2* (dotcl-simd:f64.2-aref x i)
                                                                   (dotcl-simd:f64.2-aref y i)))))
              (dotcl-simd:f64.2-horizontal+ acc))))
    (let* ((text (%flat-text (dotcl:function-sil #'%dsloc-dot)))
           (fast (subseq text 0 (search "ABUSLOW" text :from-end t)))
           (x (make-array 6 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0 5d0 6d0))))
      (list (and (search "Simd.Add.Double" fast) t)
            (and (search "Simd.Unbox" fast) t)
            (%dsloc-count "Simd.Box.Double" fast)
            (%dsloc-dot x x 6))))
  (t nil 0 91d0))

(deftest-compiled-only dotcl-simd-locals.netstandard-fasl
  (let ((src (regression-temp-file "dsloc-ns.lisp"))
        (fasl (regression-temp-file "dsloc-ns.fasl"))
        (sil (regression-temp-file "dsloc-ns.sil")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (prin1 '(defun dsloc-ns-kernel (a)
                 (let ((acc (dotcl-simd:f64.2+ a a)))
                   (declare (type dotcl-simd:f64.2 acc))
                   (setq acc (dotcl-simd:f64.2+ acc a))
                   (dotcl-simd:f64.2-values acc)))
               s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl :sil t :corlib-name "netstandard"))
    (let ((text (with-open-file (s sil) (let ((str (make-string (file-length s))))
                                          (subseq str 0 (read-sequence str s))))))
      (load fasl)
      (list (search "V128" text)
            (multiple-value-list (funcall 'dsloc-ns-kernel (dotcl-simd:make-f64.2 1d0 2d0))))))
  (nil (3d0 6d0)))

(deftest-emitting-only dotcl-simd-locals.setq-the
  (loop for name in *simd-names*
        append (%dsloc-differences
                name
                (lambda (type add xor)
                  `(let ((acc (,add a b)))
                     (declare (type ,type acc))
                     (dotimes (i 3) (setq acc (the ,type (,add acc (the ,type (,xor acc c))))))
                     (list acc (setq acc (the ,type a)) acc)))))
  nil)

(deftest-emitting-only dotcl-simd-locals.setq-the-wrong-type
  (let ((f (compile nil '(lambda (a v)
                          (let ((acc (dotcl-simd:f64.2+ a a)))
                            (declare (type dotcl-simd:f64.2 acc))
                            (setq acc (the dotcl-simd:f64.2 v))
                            acc))))
        (p (dotcl-simd:make-f64.2 1d0 2d0)))
    (list (signals-error (funcall f p 3d0) type-error)
          (signals-error (funcall f p (dotcl-simd:f32.4 1f0)) type-error)
          (multiple-value-list (dotcl-simd:f64.2-values (funcall f p (dotcl-simd:make-f64.2 5d0 6d0))))))
  (t t (5d0 6d0)))

(deftest-compiled-only dotcl-simd-locals.setq-the-shape
  (let ((dotcl:*save-sil* t))
    (eval '(defun %dsloc-the (a b)
            (let ((acc (dotcl-simd:f64.2+ a b)))
              (declare (type dotcl-simd:f64.2 acc))
              (dotimes (i 3) (setq acc (the dotcl-simd:f64.2 (dotcl-simd:f64.2* acc b))))
              (dotcl-simd:f64.2-horizontal+ acc))))
    (let ((text (%flat-text (dotcl:function-sil #'%dsloc-the))))
      (list (and (search "Simd.Mul.Double" text) t)
            (and (search "Simd.Check" text) t)
            (%dsloc-the (dotcl-simd:make-f64.2 1d0 2d0) (dotcl-simd:f64.2 2d0)))))
  (t nil 56d0))

(defun %dsloc-param-differences (name)
  (let* ((type (simd-sym name))
         (add (simd-sym name "+"))
         (xor (simd-sym name "-XOR"))
         (state (make-random-state t))
         (body `((declare (type ,type a b))
                 (let ((acc (,add a c)))
                   (declare (type ,type acc))
                   (dotimes (i 2) (setq acc (,add acc b)))
                   (setq a (,xor a acc))
                   (list a b acc (,add a b) (identity b)))))
         (bad '()))
    (eval `(defun %dsloc-param-l (a b c) ,@body))
    (eval `(defun %dsloc-param-b (a b c) (declare (notinline ,add ,xor)) ,@body))
    (dotimes (i 20)
      (let* ((args (loop repeat 3 collect (simd-random-pack name state)))
             (l (%dsl-run #'%dsloc-param-l args)) (b (%dsl-run #'%dsloc-param-b args)))
        (unless (%dsloc-same-p l b) (push (list name args l b) bad))))
    (subseq bad 0 (min 2 (length bad)))))

(deftest-emitting-only dotcl-simd-locals.params
  (loop for name in *simd-names* append (%dsloc-param-differences name))
  nil)

(deftest-emitting-only dotcl-simd-locals.params-errors-and-capture
  (progn
    (eval '(defun %dsloc-param-safe (a) (declare (type dotcl-simd:f64.2 a)) (dotcl-simd:f64.2+ a a)))
    (eval '(defun %dsloc-param-unsafe (a)
            (declare (type dotcl-simd:f64.2 a) (optimize (safety 0)))
            (dotcl-simd:f64.2+ a a)))
    (eval '(defun %dsloc-param-captured (a)
            (declare (type dotcl-simd:f64.2 a))
            (let ((get (lambda () a)))
              (list (funcall get) (dotcl-simd:f64.2+ a a)))))
    (eval '(defun %dsloc-param-special (a)
            (declare (type dotcl-simd:f64.2 a) (special a))
            (dotcl-simd:f64.2+ a a)))
    (let ((p (dotcl-simd:make-f64.2 1d0 2d0)))
      (list (signals-error (%dsloc-param-safe 3d0) type-error)
            (multiple-value-list (dotcl-simd:f64.2-values (%dsloc-param-unsafe 3d0)))
            (eq p (first (%dsloc-param-captured p)))
            (multiple-value-list (dotcl-simd:f64.2-values (second (%dsloc-param-captured p))))
            (multiple-value-list (dotcl-simd:f64.2-values (%dsloc-param-special p))))))
  (t (6d0 6d0) t (2d0 4d0) (2d0 4d0)))

(deftest-compiled-only dotcl-simd-locals.params-shape
  (let ((dotcl:*save-sil* t))
    (eval '(defun %dsloc-axpy (a x y n)
            (declare (type dotcl-simd:f64.2 a)
                     (type (simple-array double-float (*)) x y)
                     (type (integer 0 1000) n))
            (do ((i 0 (+ i 2))) ((>= i n) y)
              (declare (type (integer 0 1002) i))
              (setf (dotcl-simd:f64.2-aref y i)
                    (dotcl-simd:f64.2+ (dotcl-simd:f64.2* a (dotcl-simd:f64.2-aref x i))
                                       (dotcl-simd:f64.2-aref y i))))))
    (let ((text (%flat-text (dotcl:function-sil #'%dsloc-axpy)))
          (x (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0)))
          (y (make-array 4 :element-type 'double-float :initial-element 1d0)))
      (list (%dsloc-count "Simd.Unbox.Double" (subseq text 0 (search "ABUSLOW" text :from-end t)))
            (coerce (%dsloc-axpy (dotcl-simd:make-f64.2 2d0 3d0) x y 4) 'list))))
  (1 (3d0 7d0 7d0 13d0)))
