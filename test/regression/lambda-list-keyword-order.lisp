;;; Lambda list keywords come in the order &optional, &rest / &body, &key,
;;; &allow-other-keys, &aux, each at most once; nothing but &key and &aux may
;;; follow the &rest variable; &allow-other-keys needs &key.

(defun %llko (mode ll)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval `(lambda ,ll 1)) :ok)
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

(defmacro def-llko-bad (name ll)
  `(progn
     (deftest ,(intern (format nil "LAMBDA-LIST-KEYWORD-ORDER.~A.COMPILE" name))
       (%llko :compile ',ll) :program-error)
     (deftest ,(intern (format nil "LAMBDA-LIST-KEYWORD-ORDER.~A.INTERPRET" name))
       (%llko :interpret ',ll) :program-error)))

(def-llko-bad rest-two-vars (&rest a b))
(def-llko-bad key-before-optional (&key a &optional b))
(def-llko-bad aux-before-optional (&aux a &optional b))
(def-llko-bad optional-twice (&optional a &optional b))
(def-llko-bad key-twice (&key a &key b))
(def-llko-bad aux-twice (&aux a &aux b))
(def-llko-bad rest-twice (&rest a &rest b))
(def-llko-bad rest-then-body (&rest a &body b))
(def-llko-bad body-then-rest (&body a &rest b))
(def-llko-bad rest-after-key (&key a &rest r))
(def-llko-bad aok-alone (&allow-other-keys))
(def-llko-bad aok-after-required (a &allow-other-keys))
(def-llko-bad aok-twice (&key &allow-other-keys &allow-other-keys))
(def-llko-bad var-after-aok (&key a &allow-other-keys b))
(def-llko-bad key-after-aok (&key &allow-other-keys &key b))
(def-llko-bad key-entry-three (&key ((:a b c) 1)))
(def-llko-bad var-after-rest-and-key-order (a &rest r x &key k))

(deftest lambda-list-keyword-order.valid-full
  (let ((dotcl:*evaluator-mode* :compile))
    (funcall (eval '(lambda (a &optional (b 2) &rest r &key (k 3) ((:z zz) 4) &allow-other-keys &aux (x 5))
                     (list a b r k zz x)))
             1 2 :k 9 :other 1))
  (1 2 (:k 9 :other 1) 9 4 5))

(deftest lambda-list-keyword-order.valid-body-key-aux
  (let ((dotcl:*evaluator-mode* :compile))
    (funcall (eval '(lambda (&body b &aux (c 1)) (list b c))) 1 2))
  ((1 2) 1))

(deftest lambda-list-keyword-order.valid-key-aok-aux
  (let ((dotcl:*evaluator-mode* :compile))
    (funcall (eval '(lambda (&key a &allow-other-keys &aux (c 1)) (list a c))) :a 4 :q 1))
  (4 1))

(deftest lambda-list-keyword-order.defun-bad
  (handler-case (progn (eval '(defun %llko-bad (&optional a &optional b) a)) :ok)
    (program-error () :program-error))
  :program-error)
