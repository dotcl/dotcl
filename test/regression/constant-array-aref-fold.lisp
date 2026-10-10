;;; (AREF C I...) with C a constant variable whose value is an array and every I
;;; a literal index (or a symbol macro expanding to one, the variable of an
;;; unrolled loop) reads an element that cannot change: the reference to C may
;;; be replaced by its value (CLHS 3.2.2.3), a literal object that may not be
;;; modified. The compiler now folds the read to the element when it is a number
;;; or a character, as SBCL does. ironclad's BLAKE2 rounds read their message
;;; schedule (AREF +BLAKE2-SIGMA+ I 0) this way, each a dynamic lookup of the
;;; table and a generic two-dimensional AREF.

(setf dotcl:*save-sil* t)

(defconstant +cfa-table+
  (if (boundp '+cfa-table+)
      (symbol-value '+cfa-table+)
      (make-array '(2 3) :element-type '(unsigned-byte 8)
                         :initial-contents '((1 2 3) (4 5 6)))))

(defconstant +cfa-names+
  (if (boundp '+cfa-names+) (symbol-value '+cfa-names+) (vector "a" #\b 7)))

(defun %cfa-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %cfa-folded ()
  (list (aref +cfa-table+ 1 2)
        (symbol-macrolet ((i 1)) (aref +cfa-table+ i 0))
        (aref +cfa-names+ 1)
        (aref +cfa-names+ 2)))

(defun %cfa-string-element ()
  ;; A string element is not folded: its identity is not EQL-comparable.
  (eq (aref +cfa-names+ 0) (aref +cfa-names+ 0)))

(defun %cfa-out-of-range ()
  (aref +cfa-table+ 2 0))

(deftest cfa-folded-values
  (%cfa-folded)
  (6 4 #\b 7))

(deftest-compiled-only cfa-folded-reads-no-table
  (%cfa-sil-count #'%cfa-folded "DynamicBindings.Get")
  0)

(deftest cfa-string-element-kept
  (%cfa-string-element)
  t)

(deftest cfa-out-of-range-still-signals
  (handler-case (progn (%cfa-out-of-range) :no-error)
    (error () :error))
  :error)
