;;; An EQUALP hash table finds a key that is EQUALP to one stored, also when
;;; the key is a structure, a hash table or a pathname. Those hashed by
;;; identity, so a fresh but EQUALP key went to another bucket and GETHASH
;;; missed it. Coalton's monomorphizer keys its table of compiled
;;; specializations by structures and stopped with "slot VALUE is declared
;;; IDENTIFIER, got NIL".

(defstruct ehsk-cand name args)

(deftest equalp-hash-structure-keys.structure
  (let ((h (make-hash-table :test 'equalp)))
    (setf (gethash (make-ehsk-cand :name 'f :args (list 1 "A" (make-ehsk-cand :name 'g))) h) :found)
    (list (gethash (make-ehsk-cand :name 'f :args (list 1.0 "a" (make-ehsk-cand :name 'g))) h)
          (gethash (make-ehsk-cand :name 'f :args (list 2)) h)))
  (:found nil))

(deftest equalp-hash-structure-keys.hash-table-and-pathname
  (let ((h (make-hash-table :test 'equalp)))
    (flet ((table () (let ((x (make-hash-table))) (setf (gethash 1 x) "v") x)))
      (setf (gethash (table) h) :table
            (gethash (make-pathname :name "a" :type "b") h) :path)
      (list (gethash (table) h)
            (gethash (make-pathname :name "a" :type "b") h))))
  (:table :path))
