;;; Storing a value that is not a float of the array's own format into a
;;; DOUBLE-FLOAT or SINGLE-FLOAT array signals a TYPE-ERROR.
;;;
;;; The store converted instead: (setf (aref a 0) 1) on a double-float array
;;; stored 1.0d0, and a single-float array took 1 or 1.0d0 as 1.0. The same for
;;; FILL, REPLACE, MAKE-ARRAY's :INITIAL-ELEMENT / :INITIAL-CONTENTS,
;;; VECTOR-PUSH-EXTEND and COERCE to a float vector. ADJUST-ARRAY's
;;; :INITIAL-ELEMENT left the new elements 0.0 for any value, a symbol included.
;;; SBCL signals in all of these. A float array made without :INITIAL-ELEMENT
;;; still starts out as zeros of its format.

(defun %fes-signals (thunk)
  (handler-case (progn (funcall thunk) :stored)
    (type-error () :type-error)))

(defun %fes-generic-set (a v) (setf (aref a 0) v) (aref a 0))

(defun %fes-routes (type v)
  (list (%fes-signals (lambda () (%fes-generic-set (make-array 2 :element-type type) v)))
        (%fes-signals (lambda () (fill (make-array 2 :element-type type) v)))
        (%fes-signals (lambda () (replace (make-array 2 :element-type type) (list v))))
        (%fes-signals (lambda () (make-array 2 :element-type type :initial-element v)))
        (%fes-signals (lambda () (make-array 2 :element-type type :initial-contents (list v v))))
        (%fes-signals (lambda () (adjust-array (make-array 2 :element-type type :adjustable t)
                                               4 :initial-element v)))
        (%fes-signals (lambda () (vector-push-extend
                                  v (make-array 0 :element-type type :adjustable t
                                                  :fill-pointer 0))))
        (%fes-signals (lambda () (coerce (list v) (list 'vector type))))))

(deftest float-element-store.routes
  (list (%fes-routes 'double-float 1)
        (%fes-routes 'double-float 1.0f0)
        (%fes-routes 'double-float 'foo)
        (%fes-routes 'double-float 1.5d0)
        (%fes-routes 'single-float 1)
        (%fes-routes 'single-float 1.0d0)
        (%fes-routes 'single-float 1/2)
        (%fes-routes 'single-float 1.5f0))
  ((:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:stored :stored :stored :stored :stored :stored :stored :stored)
   (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:type-error :type-error :type-error :type-error :type-error :type-error :type-error :type-error)
   (:stored :stored :stored :stored :stored :stored :stored :stored)))

;;; A rejected ADJUST-ARRAY :INITIAL-ELEMENT leaves the array as it was.
(deftest float-element-store.adjust-rejected-keeps-array
  (let ((a (make-array 2 :element-type 'double-float :adjustable t
                         :initial-element 2d0)))
    (list (%fes-signals (lambda () (adjust-array a 4 :initial-element 1)))
          (length a) (aref a 0) (aref a 1)))
  (:type-error 2 2d0 2d0))

;;; Without :INITIAL-ELEMENT the elements are zeros of the array's format, also
;;; the ones ADJUST-ARRAY adds.
(deftest float-element-store.default-zero
  (list (aref (make-array 2 :element-type 'double-float) 1)
        (aref (make-array 2 :element-type 'single-float) 1)
        (aref (make-array '(2 2) :element-type 'double-float) 1 1)
        (aref (adjust-array (make-array 2 :element-type 'double-float :adjustable t) 4) 3)
        (aref (adjust-array (make-array 2 :element-type 'single-float) 4) 3))
  (0d0 0f0 0d0 0d0 0f0))

;;; Through a SIMPLE-ARRAY declaration, at (safety 1) and (safety 3), with the
;;; value undeclared, declared, and in a loop that fetches the buffer once.
(defun %fes-d1 (a v)
  (declare (optimize (safety 1)) (type (simple-array double-float (*)) a))
  (setf (aref a 0) v))

(defun %fes-d3 (a v)
  (declare (optimize (safety 3)) (type (simple-array double-float (*)) a))
  (setf (aref a 0) v))

(defun %fes-d1-loop (a v)
  (declare (optimize (safety 1)) (type (simple-array double-float (*)) a))
  (dotimes (i (length a)) (setf (aref a i) v)))

(defun %fes-s1-declared (a v)
  (declare (optimize (safety 1)) (type (simple-array single-float (*)) a)
           (type single-float v))
  (setf (aref a 0) v))

(defun %fes-s1-loop (a v)
  (declare (optimize (safety 1)) (type (simple-array single-float (*)) a))
  (dotimes (i (length a)) (setf (aref a i) v)))

(defun %fes-d1-fixnum (a n)
  (declare (optimize (safety 1)) (type (simple-array double-float (*)) a)
           (type fixnum n))
  (setf (aref a 0) n))

(deftest float-element-store.declared-store
  (flet ((dv () (make-array 3 :element-type 'double-float))
         (sv () (make-array 3 :element-type 'single-float)))
    (list (mapcar (lambda (f)
                    (list (%fes-signals (lambda () (funcall f (dv) 1)))
                          (%fes-signals (lambda () (funcall f (dv) 1.0f0)))
                          (let ((a (dv))) (funcall f a 2.5d0) (aref a 0))))
                  (list #'%fes-d1 #'%fes-d3 #'%fes-d1-loop))
          (mapcar (lambda (f)
                    (list (%fes-signals (lambda () (funcall f (sv) 1)))
                          (%fes-signals (lambda () (funcall f (sv) 1.0d0)))
                          (let ((a (sv))) (funcall f a 2.5f0) (aref a 0))))
                  (list #'%fes-s1-declared #'%fes-s1-loop))
          (%fes-signals (lambda () (%fes-d1-fixnum (dv) 3)))))
  (((:type-error :type-error 2.5d0)
    (:type-error :type-error 2.5d0)
    (:type-error :type-error 2.5d0))
   ((:type-error :type-error 2.5f0)
    (:type-error :type-error 2.5f0))
   :type-error))
