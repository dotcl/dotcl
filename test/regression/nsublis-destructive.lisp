;;; NSUBLIS changes the tree in place. series renames the labels of a code
;;; fragment with (NSUBLIS alist frag) and ignores the value; a copying NSUBLIS
;;; left (GO old-label) behind and the optimized loop failed with "no tagbody
;;; tag".

(deftest nsublis-modifies-in-place
  (let ((tree (list 'a (list 'go 'l) (list 'b (list 'c 'l)) 'l)))
    (nsublis '((l . m)) tree)
    tree)
  (a (go m) (b (c m)) m))

(deftest nsublis-returns-replacement-for-root
  (nsublis '((l . m)) 'l)
  m)

(deftest nsublis-dotted-tail
  (let ((tree (list* 1 2 'l)))
    (nsublis '((l . m)) tree)
    tree)
  (1 2 . m))

(deftest nsublis-matches-cons-subtree
  (let ((tree (list 1 (list 2 3) 4)))
    (nsublis '(((2 3) . x)) tree :test #'equal))
  (1 x 4))

(deftest nsublis-key-and-test-not
  (values (nsublis '((1 . one)) (list (list 1) (list 2)) :key (lambda (x) (if (consp x) (car x) x)))
          (nsublis '((a . z)) (list 'a 'b) :test-not (lambda (x y) (not (eq x y)))))
  (one (2)) (z b))

(deftest nsublis-long-list
  (let ((l (make-list 100000 :initial-element 'a)))
    (nsublis '((a . b)) l)
    (every (lambda (x) (eq x 'b)) l))
  t)
