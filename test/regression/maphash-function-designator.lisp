;;; The function given to MAPHASH is a function designator: a symbol naming a function works
;;; as #'name does (CLHS 18.2).

(defvar *mfd-seen* nil)
(defun mfd-visit (k v) (push (cons k v) *mfd-seen*))

(deftest maphash-function-designator.symbol
  (let ((h (make-hash-table)))
    (setf (gethash 1 h) 2)
    (setq *mfd-seen* nil)
    (list (maphash 'mfd-visit h) *mfd-seen*))
  (nil ((1 . 2))))

(deftest maphash-function-designator.function-and-lambda
  (let ((h (make-hash-table)) (acc nil))
    (setf (gethash :a h) 1)
    (setq *mfd-seen* nil)
    (maphash #'mfd-visit h)
    (maphash (lambda (k v) (push (list k v) acc)) h)
    (list *mfd-seen* acc))
  (((:a . 1)) ((:a 1))))

(deftest maphash-function-designator.cl-function-name
  (let ((h (make-hash-table)) (n 0))
    (setf (gethash 1 h) 2 (gethash 3 h) 4)
    (maphash 'list h)
    (maphash (lambda (k v) (declare (ignore k v)) (incf n)) h)
    n)
  2)

(deftest maphash-function-designator.errors
  (let ((h (make-hash-table)))
    (list (handler-case (maphash 'mfd-no-such-function-xyz h) (undefined-function () :undefined))
          (handler-case (progn (setf (gethash 1 h) 1) (maphash 5 h)) (type-error () :type-error))
          (handler-case (maphash 'mfd-visit 5) (type-error () :type-error))))
  (:undefined :type-error :type-error))
