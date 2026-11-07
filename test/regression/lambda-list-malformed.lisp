;;; A lambda list that names a variable twice, has &REST / &BODY with no variable, or
;;; has an &OPTIONAL / &KEY / &AUX entry with too many elements is a PROGRAM-ERROR
;;; (CLHS 3.4.1), as in SBCL, not accepted. Lambda lists that are valid and look
;;; alike are accepted.

(defun %llm-compile (lambda-list)
  (handler-case (progn (compile nil `(lambda ,lambda-list 1)) :ok)
    (program-error () :program-error)
    (error (e) (if (search "wrong number" (princ-to-string e)) :ok (type-of e)))))

(deftest-compiled-only lambda-list-malformed.duplicate-names
  (list (%llm-compile '(x x))
        (%llm-compile '(x &optional x))
        (%llm-compile '(&key a a))
        (%llm-compile '(&key (a 1 p) (b 2 p))))
  (:program-error :program-error :program-error :program-error))

(deftest-compiled-only lambda-list-malformed.empty-rest
  (list (%llm-compile '(&rest))
        (%llm-compile '(x &rest))
        (%llm-compile '(&body))
        (%llm-compile '(&rest &key a)))
  (:program-error :program-error :program-error :program-error))

(deftest-compiled-only lambda-list-malformed.too-many-elements
  (list (%llm-compile '(&optional (y 1 2 3)))
        (%llm-compile '(&key (y 1 2 3)))
        (%llm-compile '(&aux (y 1 2))))
  (:program-error :program-error :program-error))

(deftest-compiled-only lambda-list-malformed.defun-and-defmethod
  (list (handler-case (progn (eval '(defun llm-f (x x) x)) :ok) (program-error () :program-error))
        (handler-case (progn (eval '(defmethod llm-g ((x integer) x) x)) :ok) (program-error () :program-error)))
  (:program-error :program-error))

(deftest-compiled-only lambda-list-malformed.valid-ones-accepted
  (list (%llm-compile '(x y))
        (%llm-compile '(&optional (a 1) (b a)))
        (%llm-compile '(&rest r &key k))
        (%llm-compile '(&optional (a 1 a-p) &key ((:k kk) 2 k-p) &aux (z a)))
        (%llm-compile '(&key))
        (%llm-compile '(&optional))
        (%llm-compile '(&body b))
        (%llm-compile '(a &rest r))
        ;; &aux variables are bound in turn, so one may rebind a parameter (clsql does)
        (%llm-compile '(a &aux (a (list a))))
        (%llm-compile '(table &key (owner nil) &aux (table (list table)) (owner owner))))
  (:ok :ok :ok :ok :ok :ok :ok :ok :ok :ok))
