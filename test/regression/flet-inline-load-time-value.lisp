;;; A small local function is substituted at its call sites without an INLINE
;;; declaration, but not when its body (after macroexpansion) holds a
;;; LOAD-TIME-VALUE: every copy would evaluate it once (serapeum's STATIC-LET).

(defvar *filtv-n* 0)
(defmacro filtv-cell () `(load-time-value (progn (incf *filtv-n*) (list 0))))

(defun %filtv-count (form)
  (setf *filtv-n* 0)
  (let ((r (eval form)))
    (list *filtv-n* r)))

(deftest flet-inline-load-time-value.direct
  (%filtv-count '(flet ((foo () (load-time-value (incf *filtv-n*))))
                  (list (foo) (foo) (foo))))
  (1 (1 1 1)))

(deftest flet-inline-load-time-value.from-macro
  (%filtv-count '(let ((x 0))
                  (flet ((foo () (incf x) (incf (car (filtv-cell)))))
                    (list (foo) (foo) (foo) x))))
  (1 (1 2 3 3)))

;; COMPILE needs Reflection.Emit, so a build without it cannot answer this.
(deftest-emitting-only flet-inline-load-time-value.compiled
  (progn
    (setf *filtv-n* 0)
    (let ((f (compile nil '(lambda ()
                            (flet ((foo () (incf (car (filtv-cell)))))
                              (list (foo) (foo)))))))
      (list (funcall f) *filtv-n*)))
  ((1 2) 1))
