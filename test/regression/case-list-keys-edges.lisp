;;; Boundary inputs for CASE with a list of keys, around the inline EQL expansion.

(deftest case-list-keys-edges.nil-and-empty-key-lists
  (list (case nil ((nil) :single-nil) (otherwise :other))
        (case nil (nil :bare-nil) (otherwise :other))
        (case nil (() :empty) (otherwise :other))
        (case 0 (nil :bare-nil) (otherwise :other)))
  (:single-nil :other :other :other))

(deftest case-list-keys-edges.t-and-otherwise-elements
  (list (case 't ((t) :t-sym) (otherwise :other))
        (case 'otherwise ((otherwise) :otherwise-sym) (otherwise :other))
        (case 5 ((t) :t-sym) (otherwise :other))
        (case 'otherwise ((t) :t-sym) (otherwise :other)))
  (:t-sym :otherwise-sym :other :other))

(deftest case-list-keys-edges.duplicate-keys
  (list (case 1 ((1 1) :dup))
        (case 1 ((1 2 1) :dup))
        (case 1 ((1) :first) ((1) :second)))
  (:dup :dup :first))

(deftest case-list-keys-edges.fixnum-boundaries
  (list (case most-positive-fixnum ((#.most-positive-fixnum) :hit) (otherwise :miss))
        (case most-negative-fixnum ((#.most-negative-fixnum) :hit) (otherwise :miss))
        (case 0 ((0) :hit) (otherwise :miss))
        (case -0 ((0) :hit) (otherwise :miss))
        (case 9223372036854775808 ((9223372036854775808) :hit) (otherwise :miss))
        (case 9223372036854775809 ((9223372036854775808) :hit) (otherwise :miss)))
  (:hit :hit :hit :hit :hit :miss))

(deftest case-list-keys-edges.float-boundaries
  (list (case 0.0 ((0.0) :pos) (otherwise :other))
        (case -0.0 ((0.0) :pos) (otherwise :other))
        (case 0.0 ((-0.0) :neg) (otherwise :other))
        (case -0.0 ((-0.0) :neg) (otherwise :other))
        (case 1.0 ((1.0d0) :double) (otherwise :other))
        (case 1.0d0 ((1.0d0) :double) (otherwise :other))
        (case 1.0d0 ((1.0) :single) (otherwise :other)))
  (:pos :other :other :neg :other :double :other))

(deftest case-list-keys-edges.char-boundaries
  (list (case (code-char 0) ((#.(code-char 0)) :hit) (otherwise :miss))
        (case (code-char 255) ((#.(code-char 255)) :hit) (otherwise :miss))
        (case (code-char 65535) ((#.(code-char 65535)) :hit) (otherwise :miss))
        (case (code-char 1) ((#.(code-char 0)) :hit) (otherwise :miss)))
  (:hit :hit :hit :miss))

(deftest case-list-keys-edges.strings-never-eql
  (let ((s "foo"))
    (list (case s (("foo") :hit) (otherwise :miss))
          (case s (("foo" "foo") :hit) (otherwise :miss))
          (case (copy-seq "foo") (("foo") :hit) (otherwise :miss))))
  (:miss :miss :miss))

(deftest case-list-keys-edges.ecase-and-ccase
  (list (handler-case (ecase 9 (() :none))
          (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))
        (ecase 2 ((1 2) :ok))
        (let ((x 9))
          (list (handler-bind ((type-error (lambda (c) (declare (ignore c))
                                             (invoke-restart 'store-value 2))))
                  (ccase x ((1 2) :ok)))
                x)))
  ((9 (member)) :ok (:ok 2)))

(deftest case-list-keys-edges.keys-not-evaluated
  (list (case 'a ((a) :one) (otherwise :other))
        (case 'a ((quote a) :list) (otherwise :other))
        (eval '(macrolet ((keys () '(a))) (case 'a ((keys) :hit) (otherwise :miss))))
        (eval '(let ((v 'a)) (symbol-macrolet ((keyv v))
                              (case 'a ((keyv) :hit) (otherwise :miss))))))
  (:one :list :miss :miss))

(defun case-list-keys-edges-typed (x)
  (declare (ftype (function (t) fixnum) case-list-keys-edges-typed))
  (case x
    ((1) 2)
    (otherwise 3)))

(deftest case-list-keys-edges.return-type-and-effect
  (list (case-list-keys-edges-typed 1)
        (case-list-keys-edges-typed 9)
        (let ((z 0)) (case 1 ((1) (setq z 5))) z)
        (let ((z 0)) (case 2 ((1) (setq z 5))) z)
        (let ((z 0)) (case 1 (() (setq z 5))) z))
  (2 3 5 0 0))
