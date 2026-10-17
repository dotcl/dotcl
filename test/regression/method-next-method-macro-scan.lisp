;;; DEFMETHOD tells dispatch that a body cannot reach its next-method state
;;; after expanding the body's macro calls, instead of giving up at the first
;;; macro that is not CL's. A macro that expands into CALL-NEXT-METHOD or
;;; NEXT-METHOD-P still keeps the state.

(defmacro %mnms-cnm () '(call-next-method))
(defmacro %mnms-nmp () '(next-method-p))
(defmacro %mnms-wrap (x) `(let ((v ,x)) (list :wrapped v)))
(defmacro %mnms-values (a) `(values ,a (1+ ,a) (+ ,a 2) (+ ,a 3)))

(defgeneric %mnms-f (x))
(defmethod %mnms-f ((x t)) (list :t x))
(defmethod %mnms-f ((x number)) (%mnms-wrap (%mnms-cnm)))
(defmethod %mnms-f ((x integer)) (if (%mnms-nmp) (list :int (call-next-method)) :none))
(defmethod %mnms-f ((x string)) (%mnms-wrap (when (%mnms-nmp) (%mnms-cnm))))
(defmethod %mnms-f ((x symbol)) (%mnms-wrap x))

(deftest method-next-method-macro-scan.values
  (list (%mnms-f 1) (%mnms-f 1.5) (%mnms-f "s") (%mnms-f 'a) (%mnms-f #\c))
  ((:int (:wrapped (:t 1))) (:wrapped (:t 1.5)) (:wrapped (:t "s")) (:wrapped a) (:t #\c)))

(defgeneric %mnms-g (x y))
(defmethod %mnms-g ((x integer) y) (%mnms-values x))

(deftest method-next-method-macro-scan.multiple-values
  (list (multiple-value-list (%mnms-g 1 0))
        (multiple-value-bind (a b c d) (%mnms-g 5 0) (list a b c d)))
  ((1 2 3 4) (5 6 7 8)))

(defun %mnms-bytes (fn n)
  (dotimes (i 100) (funcall fn))
  (min-allocated-bytes (lambda () (dotimes (i n) (funcall fn)))))

(deftest-emitting-only method-next-method-macro-scan.no-mv-object
  (< (%mnms-bytes (lambda () (multiple-value-bind (a b c d) (%mnms-g 1 0)
                               (declare (ignore a b c d))
                               nil))
                  10000)
     4000)
  t)
