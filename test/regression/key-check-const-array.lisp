;;; The keyword-name vector a &key prologue hands to the unknown-keyword check is
;;; a constant of the function, so it is emitted once per site instead of being
;;; rebuilt on every call. That put a string[] on every &key call, including the
;;; calls that pass no keyword at all (40 B for a two-keyword function, which is
;;; the whole cost of such a call). One array is now shared by every call -- and,
;;; where the keyword sets are equal, by different functions -- so these tests pin
;;; that nothing reads it as if it were private to one call.

(defun kca-two (&key a b) (if a b 0))
(defun kca-same-keys (&key a b) (if b a 1))
(defun kca-req (x &key test) (if test x 0))
(defun kca-aok (&key a &allow-other-keys) a)

(deftest key-check-const-array.accepts-known-keys
  (list (kca-two) (kca-two :a 1 :b 2) (kca-same-keys :a 3 :b 4) (kca-req 5 :test 6))
  (0 2 3 5))

(deftest key-check-const-array.unknown-key-signals
  (handler-case (progn (kca-two :zz 1) :no-error)
    (program-error () :program-error)
    (error () :error))
  :program-error)

;;; Two functions with the same keyword names share one array; a wrong key must
;;; still be caught by each of them.
(deftest key-check-const-array.shared-array-still-checks-each-function
  (list (handler-case (progn (kca-same-keys :zz 1) :no-error)
          (error () :error))
        (handler-case (progn (kca-two :zz 1) :no-error)
          (error () :error))
        (kca-two :a 7 :b 8))
  (:error :error 8))

;;; :ALLOW-OTHER-KEYS in the call and &allow-other-keys in the lambda list both
;;; still suppress the check.
(deftest key-check-const-array.allow-other-keys
  (list (kca-two :a 1 :b 2 :zz 3 :allow-other-keys t)
        (kca-aok :a 9 :zz 1))
  (2 9))

(deftest key-check-const-array.odd-keyword-list-signals
  (handler-case (progn (kca-two :a) :no-error)
    (error () :error))
  :error)

;;; APPLY reaches the same prologue with a list built at run time.
(deftest key-check-const-array.apply-path
  (list (apply #'kca-two '(:a 1 :b 2))
        (handler-case (progn (apply #'kca-two '(:zz 1)) :no-error)
          (error () :error)))
  (2 :error))

;;; The FASL emitter has its own path for the constant (a static field filled by
;;; the type initializer, rather than the constant pool the JIT uses), so compile
;;; the same shape to a file and load it back.
(deftest-compiled-only key-check-const-array.fasl-path
  (let ((src "kca-src-tmp.lisp"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (write-string "(defun kca-fasl (&key a b) (if a b :no-a))" s)
            (terpri s)
            (write-string "(defun kca-fasl2 (&key a b) (if b a :no-b))" s))
          (load (compile-file src :output-file "kca-src-tmp.fasl"))
          (list (funcall (symbol-function 'kca-fasl) :a 1 :b 2)
                (funcall (symbol-function 'kca-fasl))
                (funcall (symbol-function 'kca-fasl2) :a 3 :b 4)
                (handler-case (progn (funcall (symbol-function 'kca-fasl) :zz 1) :no-error)
                  (error () :error))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file "kca-src-tmp.fasl"))
      (fmakunbound 'kca-fasl)
      (fmakunbound 'kca-fasl2)))
  (2 :no-a 3 :error))
