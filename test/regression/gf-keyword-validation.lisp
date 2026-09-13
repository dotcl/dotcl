;;; Validating a generic function's keyword arguments allocates nothing.
;;;
;;; CLHS 7.6.5: a call may pass any keyword some applicable method accepts, and an
;;; unknown one is a program-error. dotcl collected the accepted names into a fresh
;;; HASHSET on every call that passed a keyword -- the set, the arrays it grows, and
;;; an enumerator over the applicable-method list -- to test membership against the
;;; handful of names a method declares. 424 bytes to compare three strings.
;;;
;;; It is the shape real libraries use: cl-ppcre's SCAN is a generic function with
;;; (&key start end real-start-pos) and is called once per match, so DO-MATCHES over
;;; a 240-match string paid it 240 times.
;;;
;;; The tests that matter are the ones that still reject and still accept: a check
;;; rewritten to allocate less is only worth having if it checks the same thing.

(defgeneric gkv-op (x &key))
(defmethod gkv-op ((x integer) &key a b) (list :integer x a b))
(defmethod gkv-op ((x string) &key a (c 3)) (list :string x a c))

;;; A keyword the applicable method declares.
(deftest gf-keyword-validation.accepts-method-keywords
  (list (gkv-op 1 :a 10 :b 20)
        (gkv-op "s" :a 10 :c 30))
  ((:integer 1 10 20) (:string "s" 10 30)))

;;; The union is per applicable method, not per generic function: :C belongs to the
;;; STRING method only, and :B to the INTEGER one.
(deftest gf-keyword-validation.rejects-other-methods-keywords
  (list (handler-case (gkv-op 1 :c 1) (program-error () :program-error))
        (handler-case (gkv-op "s" :b 1) (program-error () :program-error)))
  (:program-error :program-error))

(deftest gf-keyword-validation.rejects-unknown-keyword
  (handler-case (gkv-op 1 :nope 1) (program-error () :program-error))
  :program-error)

;;; :ALLOW-OTHER-KEYS T suppresses the check; NIL does not. Only the first
;;; occurrence counts (CLHS 3.4.1.4.1).
(deftest gf-keyword-validation.allow-other-keys
  (list (handler-case (progn (gkv-op 1 :nope 1 :allow-other-keys t) :ok)
          (program-error () :program-error))
        (handler-case (gkv-op 1 :nope 1 :allow-other-keys nil)
          (program-error () :program-error))
        (handler-case (progn (gkv-op 1 :nope 1 :allow-other-keys t :allow-other-keys nil) :ok)
          (program-error () :program-error)))
  (:ok :program-error :ok))

;;; Malformed keyword portions stay program-errors.
(deftest gf-keyword-validation.malformed
  (list (handler-case (gkv-op 1 :a) (program-error () :program-error))
        (handler-case (gkv-op 1 "a" 1) (program-error () :program-error)))
  (:program-error :program-error))

;;; A method with &allow-other-keys makes any keyword acceptable.
(defgeneric gkv-open (x &key))
(defmethod gkv-open ((x integer) &key &allow-other-keys) x)

(deftest gf-keyword-validation.method-allows-other-keys
  (gkv-open 5 :whatever 1)
  5)

;;; A method with &rest and no &key takes anything too.
(defgeneric gkv-rest (x &key))
(defmethod gkv-rest ((x integer) &rest r) (length r))

(deftest gf-keyword-validation.method-rest-without-key
  (gkv-rest 5 :whatever 1)
  2)

;;; The point of the change. The method body allocates nothing, so what is left is
;;; the cost of the call and its keyword check.
(defgeneric gkv-quiet (x &key))
(defmethod gkv-quiet ((x integer) &key a b) (declare (ignore a b)) x)

(defvar *gkv-sink* nil)

(defun %gkv-call (n)
  (declare (fixnum n))
  (do ((i 0 (1+ i))) ((= i n) *gkv-sink*)
    (declare (fixnum i))
    (setq *gkv-sink* (gkv-quiet 1 :a 10))))

(defun %gkv-call-none (n)
  (declare (fixnum n))
  (do ((i 0 (1+ i))) ((= i n) *gkv-sink*)
    (declare (fixnum i))
    (setq *gkv-sink* (gkv-quiet 1))))

;;; Passing one keyword used to cost about 424 bytes more than passing none: that
;;; was the set built to check it. The bound sits between the two.
(deftest-compiled-only gf-keyword-validation.one-keyword-allocation
  (< (- (bytes-per-op #'%gkv-call) (bytes-per-op #'%gkv-call-none)) 200)
  t)

;;; And a call that passes NO keyword to a generic function that merely HAS &key
;;; used to cost 128 bytes: the applicable methods were concatenated into a fresh
;;; list before the check that would have answered immediately, because the call has
;;; no keyword portion at all. What is left is the argument array -- a generic
;;; function with &key still cannot take the loose dispatch-cache path, which is a
;;; separate cost and a separate change.
(deftest-compiled-only gf-keyword-validation.no-keyword-allocation
  (< (bytes-per-op #'%gkv-call-none) 80)
  t)

;;; The checks above run on the warm cache path, which is where they are easiest to
;;; lose: a monomorphic cache used to skip them entirely (ANSI DEFMETHOD.ERROR.14/15),
;;; and the entry now answers from its own method lists rather than a copy. Warm the
;;; entry first, then assert the same answers.
(deftest gf-keyword-validation.warm-cache-still-checks
  (progn
    (dotimes (i 50) (gkv-op 1 :a 1) (gkv-op "s" :a 1))
    (list (gkv-op 1 :a 10 :b 20)
          (handler-case (gkv-op 1 :nope 1) (program-error () :program-error))
          (handler-case (gkv-op 1 :c 1) (program-error () :program-error))
          (handler-case (progn (gkv-op 1 :nope 1 :allow-other-keys t) :ok)
            (program-error () :program-error))))
  ((:integer 1 10 20) :program-error :program-error :ok))

;;; An EQL-specialized method decides applicability from the argument, so the entry
;;; cannot answer from its class-keyed lists alone.
(defgeneric gkv-eql (x &key))
(defmethod gkv-eql ((x integer) &key a) (declare (ignore a)) :integer)
(defmethod gkv-eql ((x (eql 7)) &key a z) (declare (ignore a z)) :seven)

(deftest gf-keyword-validation.eql-method-keywords
  (progn
    (dotimes (i 50) (gkv-eql 1 :a 1) (gkv-eql 7 :a 1))
    (list (gkv-eql 7 :z 1)
          (handler-case (gkv-eql 1 :z 1) (program-error () :program-error))))
  (:seven :program-error))
