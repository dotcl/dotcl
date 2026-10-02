;;; FIND-CLASS answers are cached per symbol and dropped whenever the class
;;; table changes. These pin that the cache never outlives a change: a class
;;; defined after a miss, a class removed with (SETF FIND-CLASS) NIL, a class
;;; installed under another name, and TYPEP of an instance across those.

(defun fcc-find (name) (find-class name nil))

(deftest find-class-cache.defined-after-miss
  (progn
    (fcc-find 'fcc-late)
    (eval '(defclass fcc-late () ()))
    (and (fcc-find 'fcc-late) t))
  t)

(deftest find-class-cache.removed
  (progn
    (eval '(defclass fcc-gone () ()))
    (fcc-find 'fcc-gone)
    (setf (find-class 'fcc-gone) nil)
    (fcc-find 'fcc-gone))
  nil)

(deftest find-class-cache.renamed
  (progn
    (eval '(defclass fcc-orig () ()))
    (fcc-find 'fcc-alias)
    (setf (find-class 'fcc-alias) (find-class 'fcc-orig))
    (eq (fcc-find 'fcc-alias) (find-class 'fcc-orig)))
  t)

(deftest find-class-cache.typep-after-redefinition
  (progn
    (eval '(defstruct fcc-s a))
    (let ((s (funcall 'make-fcc-s)))
      (list (typep s 'fcc-s)
            (progn (eval '(defstruct (fcc-t (:include fcc-s)))) (typep s 'fcc-t))
            (typep (funcall 'make-fcc-t) 'fcc-s))))
  (t nil t))
