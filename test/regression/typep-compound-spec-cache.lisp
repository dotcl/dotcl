;;; TYPEP against a compound specifier written in the code: the answer is the same as
;;; interpreting the specifier each time, also after the DEFTYPEs / classes it names change.

(defvar *%tc-bytes81* (make-array 81 :element-type '(unsigned-byte 8) :initial-element 3))
(defvar *%tc-bytes80* (make-array 80 :element-type '(unsigned-byte 8) :initial-element 3))
(defvar *%tc-doubles* (make-array 4 :element-type 'double-float :initial-element 1d0))
(defvar *%tc-fixvec* (make-array 4 :initial-element 7))
(defvar *%tc-adjustable* (make-array 81 :element-type '(unsigned-byte 8) :adjustable t))
(defvar *%tc-2d* (make-array '(9 9) :element-type '(unsigned-byte 8)))

(defun %tc-all (x)
  (list (typep x '(simple-array (unsigned-byte 8) (81)))
        (typep x '(simple-array (unsigned-byte 8) (*)))
        (typep x '(simple-array double-float (*)))
        (typep x '(simple-vector 4))
        (typep x '(integer 0 80))
        (typep x '(or null (integer 0 80)))
        (typep x '(and fixnum (not (eql 5))))
        (typep x '(member 1 2 :a))
        (typep x '(eql 3))
        (typep x '(unsigned-byte 8))
        (typep x '(signed-byte 16))
        (typep x '(satisfies stringp))
        (typep x '(and vector (not string)))))

(deftest typep-compound-cache.table
  (list (%tc-all *%tc-bytes81*) (%tc-all *%tc-bytes80*) (%tc-all *%tc-doubles*)
        (%tc-all *%tc-fixvec*) (%tc-all *%tc-adjustable*) (%tc-all *%tc-2d*)
        (%tc-all 5) (%tc-all 3) (%tc-all nil) (%tc-all :a) (%tc-all "str") (%tc-all 300) (%tc-all -4)
        ;; second round over the cached tests
        (%tc-all *%tc-bytes81*) (%tc-all 5) (%tc-all *%tc-doubles*))
  ((t t nil nil nil nil nil nil nil nil nil nil t)
   (nil t nil nil nil nil nil nil nil nil nil nil t)
   (nil nil t nil nil nil nil nil nil nil nil nil t)
   (nil nil nil t nil nil nil nil nil nil nil nil t)
   (nil nil nil nil nil nil nil nil nil nil nil nil t)
   (nil nil nil nil nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil t t nil nil nil t t nil nil)
   (nil nil nil nil t t t nil t t t nil nil)
   (nil nil nil nil nil t nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil t nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil nil nil t nil)
   (nil nil nil nil nil nil t nil nil nil t nil nil)
   (nil nil nil nil nil nil t nil nil nil t nil nil)
   (t t nil nil nil nil nil nil nil nil nil nil t)
   (nil nil nil nil t t nil nil nil t t nil nil)
   (nil nil t nil nil nil nil nil nil nil nil nil t)))

;;; a type named inside the specifier changes later
(deftype %tc-thing () 'integer)
(defun %tc-thing-p (x) (typep x '(or null (%tc-thing))))
(defun %tc-listof (x) (typep x '(or null (cons (%tc-thing)))))

(deftest typep-compound-cache.deftype-redefined
  (let ((before (list (%tc-thing-p 5) (%tc-thing-p "s") (%tc-thing-p nil))))
    (eval '(deftype %tc-thing () 'string))
    (list before (list (%tc-thing-p 5) (%tc-thing-p "s") (%tc-thing-p nil))))
  ((t nil t) (nil t t)))

(deftest typep-compound-cache.satisfies-redefined
  (progn
    (eval '(defun %tc-pred (x) (evenp x)))
    (let ((a (list (typep 4 '(satisfies %tc-pred)) (typep 3 '(satisfies %tc-pred)))))
      (eval '(defun %tc-pred (x) (oddp x)))
      (list a (list (typep 4 '(satisfies %tc-pred)) (typep 3 '(satisfies %tc-pred))))))
  ((t nil) (nil t)))

;;; a malformed specifier still signals
(deftest typep-compound-cache.invalid-specifier-signals
  (list (handler-case (funcall 'typep 1 (list 'integer 'foo)) (error () :error))
        (handler-case (funcall 'typep 1 (list 'integer 'foo)) (error () :error)))
  (:error :error))
