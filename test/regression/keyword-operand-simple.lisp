;;; A keyword operand of a two-argument call is a constant and is pushed
;;; directly, like a quoted value. It used to go through the lexical-variable
;;; lookup, whose name fallback could match a local of the same name (:CELL
;;; against CELL); whether it matched depended on the package the compiler's
;;; own symbols lived in, so a compiler built by one host compiled
;;; (CONS :CELL CELL) differently from one built by another, and the bootstrap
;;; from a released dotcl stopped reproducing itself.

(setf dotcl:*save-sil* t)

(defun %kos-temp-count (fn)
  "How many BA_/BB_ operand temps FN's SIL declares."
  (let ((n 0))
    (labels ((walk (x)
               (cond ((atom x) nil)
                     ((and (eq (car x) :declare-local)
                           (symbolp (cadr x))
                           (let ((s (symbol-name (cadr x))))
                             (and (> (length s) 3)
                                  (member (subseq s 0 3) '("BA_" "BB_")
                                          :test #'string=))))
                      (incf n))
                     (t (walk (car x)) (walk (cdr x))))))
      (walk (dotcl:function-sil fn))
      n)))

(defun %kos-same-name (cell) (cons :cell cell))
(defun %kos-other-name (x) (cons :cell x))
(defun %kos-keyword-second (cell) (cons cell :cell))

(deftest-compiled-only keyword-operand-simple-temps
  (list (%kos-temp-count #'%kos-same-name)
        (%kos-temp-count #'%kos-other-name)
        (%kos-temp-count #'%kos-keyword-second))
  (0 0 0))

(deftest keyword-operand-simple-values
  (list (%kos-same-name 1) (%kos-other-name 2) (%kos-keyword-second 3))
  ((:cell . 1) (:cell . 2) (3 . :cell)))
