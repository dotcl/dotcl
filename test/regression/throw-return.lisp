;;; THROW from a function called as the value of a CATCH body returns to the
;;; CATCH instead of raising a .NET exception when the CATCH is the innermost one
;;; for the tag. These check that the shortcut keeps THROW's meaning: values,
;;; cleanups, dynamic bindings, handlers, nested catches, and the cases that
;;; still take the exception.

(defun %tr-ctak-aux (x y z)
  (declare (fixnum x y z))
  (cond ((not (< y x)) (throw '%tr-ctak z))
        (t (%tr-ctak-aux
            (catch '%tr-ctak (%tr-ctak-aux (the fixnum (1- x)) y z))
            (catch '%tr-ctak (%tr-ctak-aux (the fixnum (1- y)) z x))
            (catch '%tr-ctak (%tr-ctak-aux (the fixnum (1- z)) x y))))))

(defun %tr-ctak (x y z)
  (catch '%tr-ctak (%tr-ctak-aux x y z)))

(deftest throw-return.ctak
  (list (%tr-ctak 18 12 6) (%tr-ctak 22 12 8))
  (7 9))

(defun %tr-values (n) (if (> n 0) (throw '%tr-v (values n (* n 2) (* n 3))) :none))

(deftest throw-return.multiple-values
  (multiple-value-list (catch '%tr-v (%tr-values 4)))
  (4 8 12))

(deftest throw-return.no-throw
  (multiple-value-list (catch '%tr-v (%tr-values 0)))
  (:none))

(defvar *%tr-log* nil)

(defun %tr-cleanups (x)
  (unwind-protect
       (unwind-protect (throw '%tr-c x)
         (push :inner *%tr-log*))
    (push :outer *%tr-log*)))

(deftest throw-return.cleanups-in-order
  (let ((*%tr-log* nil))
    (list (catch '%tr-c (%tr-cleanups 5)) (reverse *%tr-log*)))
  (5 (:inner :outer)))

(defvar *%tr-special* :global)

(defun %tr-binds (x)
  (let ((*%tr-special* :bound))
    (throw '%tr-b (list x *%tr-special*))))

(deftest throw-return.special-binding-unwound
  (list (catch '%tr-b (%tr-binds 1)) *%tr-special*)
  ((1 :bound) :global))

;; A cleanup that itself catches a throw of the same shape must not overwrite
;; the value travelling to the outer catch.
(defun %tr-inner-thrower () (throw '%tr-inner :inner-value))

(defun %tr-cleanup-throws (x)
  (unwind-protect (throw '%tr-o x)
    (push (catch '%tr-inner (%tr-inner-thrower)) *%tr-log*)))

(deftest throw-return.cleanup-with-own-throw
  (let ((*%tr-log* nil))
    (list (catch '%tr-o (%tr-cleanup-throws :outer-value)) *%tr-log*))
  (:outer-value (:inner-value)))

;; A throw in a cleanup replaces the exit in progress.
(defun %tr-throw-in-cleanup ()
  (unwind-protect (throw '%tr-a :first)
    (throw '%tr-b2 :second)))

(deftest throw-return.throw-in-cleanup
  (catch '%tr-b2 (list (catch '%tr-a (%tr-throw-in-cleanup))))
  :second)

;; The innermost catch for the tag is one the callee made itself: the throw
;; goes there, not to the caller's catch.
(defun %tr-own-catch ()
  (list :after (catch '%tr-same (throw '%tr-same :mine))))

(deftest throw-return.innermost-is-callee-catch
  (catch '%tr-same (%tr-own-catch))
  (:after :mine))

;; A different tag passes through the callee's own catch.
(defun %tr-through-catch (x)
  (catch '%tr-other (throw '%tr-through x)))

(deftest throw-return.through-other-catch
  (catch '%tr-through (%tr-through-catch 11))
  11)

;; Thrown from deeper down: still reaches the catch (as an exception).
(defun %tr-deep (n) (if (= n 0) (throw '%tr-deep :bottom) (list (%tr-deep (1- n)))))

(deftest throw-return.deep
  (catch '%tr-deep (%tr-deep 5))
  :bottom)

;; The callee's own handler-bind is gone after the throw.
(defun %tr-handler (x)
  (handler-bind ((warning (lambda (c) (declare (ignore c)) (throw '%tr-h :handled))))
    (throw '%tr-h x)))

(deftest throw-return.handler-unwound
  (list (catch '%tr-h (%tr-handler :thrown))
        (catch '%tr-h (handler-case (progn (warn "w") :not-handled)
                        (warning () :outer-handler))))
  (:thrown :outer-handler))

;; Wrong argument count at the catch's call: an error, and a handler that calls
;; the function properly does not see the catch's arm.
(defun %tr-values-2 (n) (throw '%tr-v n))

(deftest throw-return.wrong-arg-count
  (handler-case (catch '%tr-v (funcall (lambda () (%tr-values-2 1 2))))
    (program-error () :program-error))
  :program-error)

(deftest throw-return.handler-calls-callee
  (catch '%tr-outer
    (handler-bind ((program-error
                     (lambda (c) (declare (ignore c))
                       (throw '%tr-outer (list :from-handler (catch '%tr-v (%tr-values-2 7)))))))
      (catch '%tr-v (%tr-values-2 1 2))))
  (:from-handler 7))

;; A tail self-call loop before the throw.
(defun %tr-loop (n acc)
  (if (= n 0) (throw '%tr-l acc) (%tr-loop (1- n) (+ acc n))))

(deftest throw-return.after-tail-loop
  (catch '%tr-l (%tr-loop 100 0))
  5050)

;; The callee reached through FUNCALL of a symbol and through APPLY.
(deftest throw-return.funcall-apply
  (list (catch '%tr-v (funcall '%tr-values-2 3))
        (catch '%tr-v (apply #'%tr-values-2 '(4))))
  (3 4))

;; No catch at all: still a CONTROL-ERROR at the throw.
(deftest throw-return.no-catcher
  (handler-case (%tr-values-2 1)
    (control-error () :control-error))
  :control-error)

;; Repeated use leaves nothing behind: a plain call after the catches.
(deftest throw-return.repeated
  (let ((sum 0))
    (dotimes (i 1000) (incf sum (catch '%tr-v (%tr-values-2 i))))
    (list sum (%tr-values 0)))
  (499500 :none))

;; Threads each have their own catches.
(require "dotcl-thread")

(deftest throw-return.threads
  (let* ((threads (loop repeat 4
                        collect (dotcl-thread:make-thread (lambda () (%tr-ctak 18 12 6)))))
         (results (mapcar #'dotcl-thread:thread-join threads)))
    results)
  (7 7 7 7))

;; Self-recursion between the catch and the throw: each level passes the
;; catch on to the next one.
(defun %tr-find (tree x)
  (cond ((eql tree x) (throw '%tr-found (list :found x)))
        ((consp tree) (%tr-find (car tree) x) (%tr-find (cdr tree) x))
        (t nil)))

(deftest throw-return.recursive-search
  (list (catch '%tr-found (%tr-find '(1 (2 (3 4)) 5) 4))
        (catch '%tr-found (%tr-find '(1 2) 9)))
  ((:found 4) nil))

(defun %tr-find-logged (tree x depth)
  (unwind-protect
       (cond ((eql tree x) (throw '%tr-found depth))
             ((consp tree) (%tr-find-logged (car tree) x (1+ depth))
              (%tr-find-logged (cdr tree) x (1+ depth)))
             (t nil))
    (push depth *%tr-log*)))

(deftest throw-return.recursive-cleanups
  (let ((*%tr-log* nil))
    (list (catch '%tr-found (%tr-find-logged '((a)) 'a 0)) *%tr-log*))
  (2 (0 1 2)))

;; A level in between catches the same tag itself.
(defun %tr-nest (n)
  (cond ((= n 0) (throw '%tr-n :bottom))
        ((= n 2) (list :caught (catch '%tr-n (%tr-nest (1- n)))))
        (t (list n (%tr-nest (1- n))))))

(deftest throw-return.recursive-inner-catch
  (catch '%tr-n (%tr-nest 4))
  (4 (3 (:caught :bottom))))

;; The recursion reached without a catch around it still signals at the throw.
(deftest throw-return.recursive-no-catcher
  (handler-case (%tr-find '(1 (2)) 2)
    (control-error () :control-error))
  :control-error)
