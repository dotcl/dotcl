;;; A local function declared INLINE, substituted where its name is also a
;;; variable, where a macro shares a parameter's name, and where its body asks a
;;; macro's &ENVIRONMENT about a parameter bound to a literal.

(deftest flet-inline-scope.variable-of-same-name
  (let ((f 5))
    (labels ((f (x) (* x 10)))
      (declare (inline f))
      (+ (funcall (lambda () f))
         (funcall (lambda () (f 3))))))
  35)

(defvar *fis-expand-count* 0)

(defmacro fis-marker (&rest args)
  (declare (ignore args))
  (incf *fis-expand-count*)
  ''expanded)

(defun %fis-marker-param (x)
  (flet ((pick (fis-marker) (list fis-marker)))
    (declare (inline pick))
    (pick x)))

(deftest flet-inline-scope.param-named-like-macro
  (list (%fis-marker-param 'a) *fis-expand-count*)
  ((a) 0))

(defmacro fis-probe (name &environment env)
  `',(multiple-value-list (macroexpand-1 name env)))

(deftest flet-inline-scope.literal-arg-seen-by-macro
  (flet ((g (fis-x) (list fis-x (fis-probe fis-x))))
    (declare (inline g))
    (g 1))
  (1 (fis-x nil)))
