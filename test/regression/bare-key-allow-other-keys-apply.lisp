;;; A &KEY &ALLOW-OTHER-KEYS lambda list with no key parameters takes any number of
;;; keyword pairs, called directly, through FUNCALL and through APPLY.
(defun %bkao-1 (a &key &allow-other-keys) (list a))
(defun %bkao-2 (a &optional (b 7) &key &allow-other-keys) (list a b))
(defun %bkao-3 (a &key) (list a))

(deftest bare-key-allow-other-keys.apply
  (list (apply #'%bkao-1 '(1 :b 2 :c 3))
        (apply #'%bkao-1 '(1 :b 2 :c 3 :d 4))
        (apply #'%bkao-1 '(1 :b 2 :b 3))
        (apply #'%bkao-1 '(1 :allow-other-keys nil :x 1))
        (funcall '%bkao-1 1 :b 2 :c 3)
        (%bkao-1 1 :b 2 :c 3)
        (%bkao-1 1 :b 2)
        (%bkao-1 1))
  ((1) (1) (1) (1) (1) (1) (1) (1)))

(deftest bare-key-allow-other-keys.odd-list
  (list (handler-case (apply #'%bkao-1 '(1 :b)) (error () :error))
        (handler-case (apply #'%bkao-1 '(1 :b 2 :c)) (error () :error))
        (handler-case (apply #'%bkao-3 '(1 :b 2)) (error () :error))
        (apply #'%bkao-3 '(1)))
  (:error :error :error (1)))

(defun %bkao-4 (a &rest r &key &allow-other-keys) (list a r))
(defun %bkao-5 (&rest r &key &allow-other-keys) r)

(deftest bare-key-allow-other-keys.rest
  (list (%bkao-4 1)
        (%bkao-4 1 :b 2 :c 3)
        (apply #'%bkao-4 '(1 :allow-other-keys t :x 1))
        (%bkao-5)
        (%bkao-5 :a 1)
        (handler-case (%bkao-4 1 :b) (error () :error)))
  ((1 nil) (1 (:b 2 :c 3)) (1 (:allow-other-keys t :x 1)) nil (:a 1) :error))
