;;; What is known about an array's type reaches the code that uses it: a
;;; closure that captures the array, a declaration without a rank (the rank is
;;; that of the AREFs), and (MAKE-ARRAY N :ELEMENT-TYPE (ARRAY-ELEMENT-TYPE X)).
;;; The element buffer fetched for these declines for any other array, so the
;;; values and the errors are those of the per-element path.

(setf dotcl:*save-sil* t)

(defun %ati-capture (px)
  (declare (type (simple-array (unsigned-byte 32)) px))
  (let ((p px))
    (lambda (k)
      (let ((s 0))
        (dotimes (i k) (incf s (aref p (mod i 2) i)))
        s))))

(defun %ati-capture-write (px)
  (declare (type (simple-array (unsigned-byte 32)) px))
  (lambda (v)
    (dotimes (i 2) (setf (aref px 0 i) v))
    (aref px 0 1)))

(defun %ati-capture-setq (v)
  (let ((a (make-array 4 :element-type 'fixnum :initial-element 1)))
    (let ((f (lambda () (let ((s 0)) (dotimes (i 2) (incf s (aref a i))) s))))
      (setq a v)
      (funcall f))))

(defun %ati-capture-setq-inside (v)
  (let ((a (make-array 4 :element-type 'fixnum :initial-element 1)))
    (funcall (lambda () (let ((s 0)) (dotimes (i 2) (incf s (aref a i))) (setq a v) s)))
    (list (aref a 0) (aref a 1))))

(defun %ati-rank (a n)
  (declare (type (simple-array fixnum) a))
  (let ((s 0)) (dotimes (i n) (incf s (aref a i i))) s))

(defun %ati-mixed-rank (a)
  (declare (type (simple-array fixnum) a))
  (let ((s 0))
    (dotimes (i 2) (incf s (if (= (array-rank a) 1) (aref a i) (aref a i i))))
    (list s (row-major-aref a 1))))

(defun %ati-let-decl (x)
  (let ((a x))
    (declare (type (simple-array (unsigned-byte 8)) a))
    (let ((s 0)) (dotimes (i 1) (incf s (aref a i i))) s)))

(defun %ati-like (src v)
  (declare (type (simple-array (unsigned-byte 32) (*)) src))
  (let ((d (make-array 4 :element-type (array-element-type src))))
    (dotimes (i 4) (setf (aref d i) (aref src i)))
    (setf (aref d 0) v)
    (list (array-element-type d) (aref d 0) (aref d 3))))

(defun %ati-like-captured (px)
  (declare (type (simple-array (unsigned-byte 8)) px))
  (lambda (y)
    (let ((nb (make-array 3 :element-type (array-element-type px))))
      (dotimes (i 3) (setf (aref nb i) (aref px y i)))
      (+ (aref nb 0) (aref nb 1) (aref nb 2)))))

(defun %ati-2d (rows cols contents &optional (type '(unsigned-byte 32)))
  (make-array (list rows cols) :element-type type :initial-contents contents))

(deftest array-type-inference.values
  (list (funcall (%ati-capture (%ati-2d 2 3 '((1 2 3) (4 5 6)))) 3)
        (funcall (%ati-capture (make-array 3 :element-type '(unsigned-byte 32))) 0)
        (funcall (%ati-capture-write (%ati-2d 1 2 '((1 2)))) 7)
        (%ati-capture-setq (vector 10 20))
        (%ati-capture-setq-inside (vector 10 20))
        (%ati-rank (%ati-2d 2 2 '((1 2) (3 4)) 'fixnum) 2)
        (%ati-rank (make-array 3 :element-type 'fixnum) 0)
        (%ati-mixed-rank (make-array 2 :element-type 'fixnum :initial-contents '(5 6)))
        (%ati-mixed-rank (%ati-2d 2 2 '((1 2) (3 4)) 'fixnum))
        (%ati-like (make-array 4 :element-type '(unsigned-byte 32)
                                 :initial-contents '(1 2 3 4))
                   9)
        (funcall (%ati-like-captured (%ati-2d 2 3 '((1 2 3) (4 5 6)) '(unsigned-byte 8))) 1))
  (9 0 7 30 (10 20) 5 0 (11 6) (5 2) ((unsigned-byte 32) 9 4) 15))

(deftest array-type-inference.errors
  (flet ((e (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error () :type-error)
             (error () :error)))
         (any-error (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (error () :error))))
    (list
     ;; A rank-1 array where the AREFs take two subscripts.
     (any-error (lambda () (funcall (%ati-capture (make-array 3 :element-type '(unsigned-byte 32))) 1)))
     (any-error (lambda () (%ati-rank (make-array 3 :element-type 'fixnum) 1)))
     ;; Not the declared array at all.
     (e (lambda () (%ati-capture (%ati-2d 1 1 '((1)) 'fixnum))))
     (e (lambda () (%ati-rank (make-array '(1 1) :initial-element 1) 1)))
     (e (lambda () (%ati-rank (make-array '(1 1) :element-type 'fixnum :adjustable t) 1)))
     ;; A store outside the element type of the array MAKE-ARRAY made.
     (e (lambda () (%ati-like (make-array 4 :element-type '(unsigned-byte 32)) (expt 2 32))))
     (e (lambda () (%ati-like (make-array 4 :element-type '(unsigned-byte 32)) -1)))
     (e (lambda () (funcall (%ati-capture-write (%ati-2d 1 2 '((1 2)))) (expt 2 32))))))
  (:error :error :type-error :type-error :type-error :type-error :type-error :type-error))

;;; The tree-walk evaluator of the emit-free build does not check a LET's
;;; declarations.
(deftest-emitting-only array-type-inference.let-declaration
  (list (handler-case (%ati-let-decl (make-array '(1 1) :initial-element 1000))
          (type-error () :type-error))
        (%ati-let-decl (%ati-2d 1 1 '((200)) '(unsigned-byte 8))))
  (:type-error 200))

(defun %ati-sil-has (fn text) (and (search text (%flat-text (dotcl:function-sil fn))) t))

(deftest-compiled-only array-type-inference.fetched
  (list (%ati-sil-has #'%ati-capture "Runtime.BackingIfI64")
        (%ati-sil-has #'%ati-rank "Runtime.BackingIfI64")
        (%ati-sil-has #'%ati-like "Runtime.BackingIfI64")
        (%ati-sil-has #'%ati-like-captured "Runtime.BackingIfU8")
        (%ati-sil-has #'%ati-mixed-rank "Runtime.BackingIf"))
  (t t t t nil))

(setf dotcl:*save-sil* nil)
