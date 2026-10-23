;;; A parameter declared with a DEFTYPE that expands to a compound of
;;; UNSIGNED-BYTE (Coalton's UFIXNUM is (and fixnum unsigned-byte)) accepts the
;;; non-negative integers and rejects everything else.

(deftype %dub-ufix () '(and fixnum unsigned-byte))
(deftype %dub-uint () '(or null unsigned-byte))

(defun %dub-ufix-fn (x) (declare (type %dub-ufix x)) (list x))
(defun %dub-uint-fn (x) (declare (type %dub-uint x)) (list x))

(defun %dub-te (thunk)
  (handler-case (list :value (funcall thunk))
    (type-error () :type-error)))

(deftest deftype-unsigned-byte-arg-check.ufixnum
  (mapcar (lambda (v) (%dub-te (lambda () (%dub-ufix-fn v))))
          (list 0 7 most-positive-fixnum -1 (1+ most-positive-fixnum) 1.0 1/2 nil #\a))
  ((:value (0)) (:value (7)) (:value (#.most-positive-fixnum))
   :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest deftype-unsigned-byte-arg-check.unsigned-byte
  (mapcar (lambda (v) (%dub-te (lambda () (%dub-uint-fn v))))
          (list nil 0 (expt 2 100) -1 (- (expt 2 100)) 2.0d0))
  ((:value (nil)) (:value (0)) (:value (#.(expt 2 100)))
   :type-error :type-error :type-error))

(deftest deftype-unsigned-byte-arg-check.typep
  (list (typep 5 '%dub-ufix) (typep -5 '%dub-ufix) (typep (expt 2 70) '%dub-ufix)
        (typep (expt 2 70) '%dub-uint) (typep (- (expt 2 70)) '%dub-uint)
        (typep 3 '(and integer unsigned-byte)) (typep -3 '(and integer unsigned-byte)))
  (t nil nil t nil t nil))
