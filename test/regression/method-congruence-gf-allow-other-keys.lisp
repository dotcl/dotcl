;;; CLHS 7.6.4 rule 4 (a method must accept every keyword named after &key in
;;; the generic function lambda list) is not applied when the generic function
;;; lambda list itself has &allow-other-keys, as in SBCL. Keyword checking is
;;; then off for every call (CLHS 7.6.5), so such a method still works.
;;; generic-comparability defines EQUALS this way.

(defun mcgaok-try (gf-ll m-ll &optional inline)
  (let ((name (gensym "MCGAOK")))
    (handler-case
        (progn
          (if inline
              (eval `(defgeneric ,name ,gf-ll
                       (:method ,(cons '(a t) (cdr m-ll)) (declare (ignorable a)) :ok)))
              (progn
                (eval `(defgeneric ,name ,gf-ll))
                (eval `(defmethod ,name ,(cons '(a t) (cdr m-ll))
                         (declare (ignorable a)) :ok))))
          :accept)
      (program-error () :error))))

(deftest method-congruence-gf-aok.empty-key
  (mcgaok-try '(a &key key &allow-other-keys) '(a &key))
  :accept)

(deftest method-congruence-gf-aok.other-key
  (mcgaok-try '(a &key key &allow-other-keys) '(a &key other))
  :accept)

(deftest method-congruence-gf-aok.rest-and-other-key
  (mcgaok-try '(a &rest r &key recursive key &allow-other-keys)
              '(a &rest keys &key (case-sensitive t)))
  :accept)

(deftest method-congruence-gf-aok.inline-method
  (mcgaok-try '(a &key key &allow-other-keys) '(a &key) t)
  :accept)

;;; Without &allow-other-keys on the generic function the rule still applies.
(deftest method-congruence-gf-aok.no-aok-still-error
  (list (mcgaok-try '(a &key key) '(a &key))
        (mcgaok-try '(a &key key) '(a &key other))
        (mcgaok-try '(a &key key) '(a &rest r &key other))
        (mcgaok-try '(a &key key) '(a &key) t))
  (:error :error :error :error))

;;; Rules 1-3 are unchanged by &allow-other-keys.
(deftest method-congruence-gf-aok.no-key-still-error
  (mcgaok-try '(a &key key &allow-other-keys) '(a))
  :error)

(deftest method-congruence-gf-aok.call
  (let ((name (gensym "MCGAOK")))
    (eval `(defgeneric ,name (a &key key &allow-other-keys)))
    (eval `(defmethod ,name ((a t) &key) (list a)))
    (eval `(defmethod ,name ((a string) &rest keys &key (case-sensitive t))
             (list a keys case-sensitive)))
    (list (funcall name 1 :key 2 :zz 3)
          (funcall name "s" :key 2 :case-sensitive nil)))
  ((1) ("s" (:key 2 :case-sensitive nil) nil)))
