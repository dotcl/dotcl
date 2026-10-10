;;; DOTCL:SYMBOL-GLOBAL-VALUE reads and SETF of it writes the global value of a
;;; symbol, ignoring dynamic bindings in effect (like SB-EXT:SYMBOL-GLOBAL-VALUE
;;; on SBCL). Libraries such as hu.dwim.util use it where SBCL has it.

(defvar *sgv-var* :global)
(defvar *sgv-unbound*)

(deftest symbol-global-value-outside-binding
  (dotcl:symbol-global-value '*sgv-var*)
  :global)

(deftest symbol-global-value-inside-binding
  (let ((*sgv-var* :dynamic))
    (list *sgv-var* (dotcl:symbol-global-value '*sgv-var*) (symbol-value '*sgv-var*)))
  (:dynamic :global :dynamic))

(deftest symbol-global-value-setf-inside-binding
  (let ((r (let ((*sgv-var* :dynamic))
             (list (setf (dotcl:symbol-global-value '*sgv-var*) :new-global)
                   *sgv-var*
                   (dotcl:symbol-global-value '*sgv-var*)))))
    (prog1 (list r *sgv-var*)
      (setf *sgv-var* :global)))
  ((:new-global :dynamic :new-global) :new-global))

(deftest symbol-global-value-progv
  (progv '(*sgv-var*) '(:progv)
    (list (symbol-value '*sgv-var*) (dotcl:symbol-global-value '*sgv-var*)))
  (:progv :global))

(deftest symbol-global-value-unbound-globally
  (let ((*sgv-unbound* :dynamic))
    (handler-case (dotcl:symbol-global-value '*sgv-unbound*)
      (unbound-variable (c) (list :unbound (cell-error-name c)))))
  (:unbound *sgv-unbound*))

(deftest symbol-global-value-unbound-not-special
  (handler-case (dotcl:symbol-global-value (make-symbol "SGV-FRESH"))
    (unbound-variable () :unbound))
  :unbound)

(deftest symbol-global-value-funcall-setf
  (let ((s (make-symbol "SGV-FRESH")))
    (list (funcall #'(setf dotcl:symbol-global-value) 3 s)
          (dotcl:symbol-global-value s)
          (symbol-value s)))
  (3 3 3))

(deftest symbol-global-value-constant
  (list (dotcl:symbol-global-value 'pi)
        (handler-case (progn (setf (dotcl:symbol-global-value 'pi) 3) :set)
          (error () :error)))
  (#.pi :error))

(deftest symbol-global-value-not-a-symbol
  (handler-case (dotcl:symbol-global-value 42)
    (type-error () :type-error))
  :type-error)
