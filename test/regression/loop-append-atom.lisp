;;; LOOP APPEND treats a piece the way APPEND treats its last argument: an atom as the last
;;; piece is the tail of the result (and a lone atom is the result), anything after an atom or
;;; after a dotted piece is an error. NCONC is unchanged. The expected values are SBCL's.

(defun %laa (kind items)
  (handler-case
      (ecase kind
        (:append (loop for x in items append x))
        (:append-into (loop for x in items append x into r finally (return (list r :done))))
        (:nconc (loop for x in items nconc x)))
    (error () :error)))

(deftest loop-append-atom.values
  (list (%laa :append (list 6))
        (%laa :append (list (list 1) 6))
        (%laa :append (list nil 5))
        (%laa :append (list (list 1 2) nil 5))
        (%laa :append (list 1 2 6))
        (%laa :append (list (list 1) 6 (list 2)))
        (%laa :append (list (cons 1 2) (list 'a)))
        (%laa :append (list (cons 1 2)))
        (%laa :append (list (cons 1 2) nil 5))
        (%laa :append (list nil nil))
        (%laa :append nil)
        (%laa :append-into (list (list 1) 5))
        (%laa :append-into (list 5 (list 1)))
        (%laa :nconc (list (list 1) 6))
        (%laa :nconc (list (list 1) (list 2 3))))
  (6 (1 . 6) 5 (1 2 . 5) :error :error :error (1 . 2) :error nil nil ((1 . 5) :done) :error :error (1 2 3)))
