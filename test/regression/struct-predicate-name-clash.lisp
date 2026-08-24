;;; A structure with a slot named P has, under the default conc-name, an accessor
;;; with the same name as the default predicate: both are <NAME>-P. ANSI does not
;;; say which one wins. SBCL keeps the accessor and warns, and so does dotcl.
;;;
;;; Loading this file prints that warning once per structure below; that is the
;;; behaviour under test, not a problem with the run.
;;;
;;; What made this worth pinning is that dotcl used to answer differently
;;; depending on the build. A compiled call site inlines the accessor from the
;;; DEFSTRUCT-populated registry, so compiled code read the slot, while the
;;; predicate DEFUN was what an interpreted call (and #'NAME) reached. The
;;; emit-free build has no compiler, so it read T where the compiled build read
;;; the slot.

(defstruct spc p q)

(deftest struct-predicate-clash-accessor-wins
  (let ((s (make-spc :p 1 :q 2)))
    (list (spc-p s) (spc-q s)))
  (1 2))

;;; Same answer when the call cannot be inlined: through #', through APPLY, and
;;; through a variable holding the function.
(deftest struct-predicate-clash-indirect-calls
  (let ((s (make-spc :p 1 :q 2))
        (f (symbol-function 'spc-p)))
    (list (funcall #'spc-p s) (apply #'spc-p (list s)) (funcall f s)))
  (1 1 1))

;;; The structure is still recognisable as its type, and SETF of the accessor
;;; still writes the slot.
(deftest struct-predicate-clash-type-and-setf
  (let ((s (make-spc :p 1 :q 2)))
    (setf (spc-p s) :new)
    (list (spc-p s) (typep s 'spc) (typep 42 'spc)))
  (:new t nil))

;;; Naming the predicate explicitly removes the ambiguity: both exist.
(defstruct (spc2 (:predicate spc2-structure-p)) p q)

(deftest struct-predicate-clash-explicit-predicate-name
  (let ((s (make-spc2 :p 1 :q 2)))
    (list (spc2-p s) (spc2-structure-p s) (spc2-structure-p 42)))
  (1 t nil))

;;; (:predicate nil) asks for no predicate at all, which is not a clash either.
(defstruct (spc3 (:predicate nil)) p q)

(deftest struct-predicate-clash-suppressed-predicate
  (let ((s (make-spc3 :p 1 :q 2)))
    (list (spc3-p s) (spc3-q s)))
  (1 2))
