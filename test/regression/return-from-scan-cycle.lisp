;;; A DEFUN body's scan for (RETURN-FROM name) expands macros. A macro whose
;;; expansion is a MACROLET that rebinds the macro's own name (as CLX's
;;; WITH-BUFFER does) leads the scan back to a form it is already scanning; it
;;; is scanned once, not again at every expansion depth up to the limit.

(eval `(defmacro rfsc-with (&body body)
         (list* 'macrolet
                '((rfsc-with (&body b) ,@(make-list 15000 :initial-element 0)
                             (cons 'progn b)))
                body)))

(deftest return-from-scan-cycle
  (let ((t0 (get-internal-real-time)))
    (eval '(defun rfsc-use (x) (rfsc-with (setq x (1+ x))) x))
    (list (funcall 'rfsc-use 1)
          (< (- (get-internal-real-time) t0) (* 2 internal-time-units-per-second))))
  (2 t))
