;;; Tree and list operations on a list too long for one stack frame per element.
(defvar *hl-n* 2000000)

(deftest huge-list-tree-operations.copy-tree
  (let* ((l (make-list *hl-n* :initial-element 1))
         (c (copy-tree l)))
    (list (length c) (eq c l) (tree-equal c l)))
  (2000000 nil t))

(deftest huge-list-tree-operations.subst
  (let* ((l (make-list *hl-n* :initial-element 1))
         (s (subst 2 1 l))
         (same (subst 2 3 l)))
    (list (length s) (car s) (car (last s)) (eq same l)
          (length (sublis '((1 . 5)) l))
          (length (subst-if 7 #'numberp l))
          (cdr (last (subst-if-not 7 #'consp l)))))
  (2000000 2 2 t 2000000 2000000 7))

(deftest huge-list-tree-operations.nsubst
  (let* ((l (make-list *hl-n* :initial-element 1))
         (r (nsubst 2 1 l)))
    (list (eq r l) (car l) (car (last l))
          (length (nsublis '((2 . 3)) l))
          (car (last l))
          (length (nsubst-if 4 #'numberp l))
          (cdr (last (nsubst-if-not 5 #'consp l)))))
  (t 2 2 2000000 3 2000000 5))

(deftest huge-list-tree-operations.nconc
  (let* ((a (make-list *hl-n* :initial-element 1))
         (b (make-list *hl-n* :initial-element 2))
         (r (nconc a nil b nil)))
    (list (length r) (eq r a) (car (last r)) (nth *hl-n* r)))
  (4000000 t 2 2))

(deftest huge-list-tree-operations.small-tree-sharing
  (let* ((tree (list 'a (list 'b 'c) (list 'd (list 'e))))
         (same (subst 'x 'zz tree))
         (changed (subst 'x 'c tree)))
    (list (eq same tree) changed (eq (third changed) (third tree))
          (eq (second changed) (second tree))))
  (t (a (b x) (d (e))) t nil))

(deftest huge-list-tree-operations.nconc-first-args
  (list (nconc) (nconc nil) (nconc nil nil) (nconc nil (list 1) nil (list 2) 3)
        (nconc (list 1) 2))
  (nil nil nil (1 2 . 3) (1 . 2)))

(deftest huge-list-tree-operations.nconc-atom-arguments
  (flet ((try (f) (handler-case (funcall f) (error () :error))))
    (list (try (lambda () (nconc (list 1) 7 nil)))
          (try (lambda () (nconc nil 7 nil)))
          (try (lambda () (nconc 7 nil)))
          (try (lambda () (nconc (list 1) 7 (list 2))))
          (try (lambda () (nconc 7 (list 1))))
          (try (lambda () (nconc (list 1) nil 7)))
          (try (lambda () (nconc (list 1) 7 8)))
          (try (lambda () (nconc nil nil 7)))))
  (:error :error :error :error :error (1 . 7) :error 7))
