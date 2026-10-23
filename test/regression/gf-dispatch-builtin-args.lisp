;;; Conses, NIL, symbols (T among them) and fixnums dispatch on the class CLASS-OF
;;; gives them, in either argument position, on a cold and on a warm cache.

(defgeneric %gdba1 (x))
(defmethod %gdba1 ((x cons)) :cons)
(defmethod %gdba1 ((x list)) :list)
(defmethod %gdba1 ((x null)) :null)
(defmethod %gdba1 ((x symbol)) :symbol)
(defmethod %gdba1 ((x fixnum)) :fixnum)
(defmethod %gdba1 ((x integer)) :integer)
(defmethod %gdba1 (x) (declare (ignore x)) :t)

(defgeneric %gdba2 (x y))
(defmethod %gdba2 ((x cons) (y symbol)) :cons-symbol)
(defmethod %gdba2 ((x null) (y fixnum)) :null-fixnum)
(defmethod %gdba2 ((x symbol) (y cons)) :symbol-cons)
(defmethod %gdba2 ((x fixnum) (y null)) :fixnum-null)
(defmethod %gdba2 ((x list) y) (declare (ignore y)) :list-t)
(defmethod %gdba2 (x (y list)) (declare (ignore x)) :t-list)
(defmethod %gdba2 (x y) (declare (ignore x y)) :t-t)

(defun %gdba-objects ()
  (list (list 1) nil 'foo t :kw 3 (expt 2 70) "s" #\c))

(defun %gdba-run ()
  (let ((objs (%gdba-objects)))
    (list (mapcar #'%gdba1 objs)
          (loop for x in objs
                collect (loop for y in objs collect (%gdba2 x y))))))

(deftest gf-dispatch-builtin-args.one-arg
  (loop repeat 3 collect (first (%gdba-run)))
  (#1=(:cons :null :symbol :symbol :symbol :fixnum :integer :t :t) #1# #1#))

(deftest gf-dispatch-builtin-args.two-args
  (let ((runs (loop repeat 50 collect (second (%gdba-run)))))
    (list (every (lambda (r) (equal r (first runs))) runs)
          (mapcar (lambda (row) (subseq row 0 6)) (subseq (first runs) 0 6))))
  (t ((:list-t :cons-symbol :cons-symbol :cons-symbol :cons-symbol :list-t)
      (:symbol-cons :list-t :list-t :list-t :list-t :null-fixnum)
      (:symbol-cons :t-list :t-t :t-t :t-t :t-t)
      (:symbol-cons :t-list :t-t :t-t :t-t :t-t)
      (:symbol-cons :t-list :t-t :t-t :t-t :t-t)
      (:t-list :fixnum-null :t-t :t-t :t-t :t-t))))
