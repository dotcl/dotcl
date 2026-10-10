;;; A method whose body cannot reach CALL-NEXT-METHOD or NEXT-METHOD-P is called by
;;; dispatch without publishing its next-method state. DEFMETHOD decides that by
;;; walking the body, so every way a body can reach them without naming them in
;;; plain sight has to keep the state: a global macro, a MACROLET or
;;; SYMBOL-MACROLET around the DEFMETHOD (compiled or interpreted), a global
;;; symbol macro, and the function name used as data.

(defgeneric gnms (x))
(defmethod gnms ((x real)) :real)
(defmethod gnms ((x t)) :t)

(defmacro %gnms-next () '(call-next-method))
(defmethod gnms ((x string)) (list :string (%gnms-next)))

(macrolet ((nx () '(call-next-method)))
  (defmethod gnms ((x float)) (list :float (nx))))

(defun %gnms-define-integer ()
  (macrolet ((nx () '(call-next-method)))
    (defmethod gnms ((x integer)) (list :integer (nx)))))
(%gnms-define-integer)

(symbol-macrolet ((nx (call-next-method)))
  (defmethod gnms ((x ratio)) (list :ratio nx)))

(define-symbol-macro %gnms-next-sm (call-next-method))
(defmethod gnms ((x symbol)) (list :symbol %gnms-next-sm))

(let ((dotcl:*evaluator-mode* :interpret))
  (eval '(macrolet ((nx () '(call-next-method)))
          (defmethod gnms ((x cons)) (list :cons (nx))))))

(defmethod gnms ((x character)) (list :character (funcall 'call-next-method)))

(defmacro %gnms-has-next () '(next-method-p))
(defmethod gnms ((x vector)) (list :vector (%gnms-has-next)))

;; A method that ignores the state, first in a chain of two, and one that calls
;; another generic function whose method does use it.
(defmethod gnms ((x hash-table)) (list :hash-table (gnms 1.5)))

(defvar *gnms-args* (list 1.5 3 1/2 "s" 'a '(1) #\c (vector 1) (make-hash-table)))

(deftest gf-next-method-state.reached-through-macros
  (progn
    ;; More classes than the front cache holds, twice, so both the first call
    ;; and the cached entries (front and table) are exercised.
    (mapcar #'gnms *gnms-args*)
    (mapcar #'gnms *gnms-args*))
  ((:float :real) (:integer :real) (:ratio :real) (:string (:vector t)) (:symbol :t)
   (:cons :t) (:character :t) (:vector t) (:hash-table (:float :real))))
