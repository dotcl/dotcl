;;; A generic function with &key, called without a keyword, takes the same
;;; argument-array-free dispatch as one without &key. There is no keyword to
;;; check on such a call. A call that passes a keyword still goes through the
;;; full dispatcher, which checks it.

(defvar *gkn-log* '())

(defgeneric %gkn-f (a b &key start end))
(defmethod %gkn-f ((a function) b &key (start 0) (end (length b)))
  (funcall a b start end))
(defmethod %gkn-f ((a symbol) b &key (start 0) (end (length b)))
  (list a b start end))
(defmethod %gkn-f ((a integer) b &key start end)
  (list :int (call-next-method) a b start end))
(defmethod %gkn-f ((a number) b &key start end)
  (list :num a b start end))
(defmethod %gkn-f :before ((a string) b &key start end)
  (push (list :before a b start end) *gkn-log*))
(defmethod %gkn-f ((a string) b &key (start 1) end)
  (list :string a b start end))

(defgeneric %gkn-opt (a &optional b &key c))
(defmethod %gkn-opt ((a symbol) &optional (b :b) &key (c :c)) (list a b c))

(deftest gf-key-no-keyword-dispatch.values
  (list (%gkn-f 'x "ab")
        (%gkn-f 'x "ab" :end 1)
        (%gkn-f 1 2)
        (%gkn-f 1 2 :start 3)
        (%gkn-f 1.5 2)
        (%gkn-f #'list "abc")
        (%gkn-f #'list "abc" :start 2)
        (%gkn-opt 'a)
        (%gkn-opt 'a 1)
        (%gkn-opt 'a 1 :c 2))
  ((x "ab" 0 2) (x "ab" 0 1) (:int (:num 1 2 nil nil) 1 2 nil nil)
   (:int (:num 1 2 3 nil) 1 2 3 nil) (:num 1.5 2 nil nil)
   ("abc" 0 3) ("abc" 2 3) (a :b :c) (a 1 :c) (a 1 2)))

(deftest gf-key-no-keyword-dispatch.before
  (progn
    (setf *gkn-log* '())
    (list (%gkn-f "s" 1)
          (%gkn-f "s" 1 :end 2)
          (reverse *gkn-log*)))
  ((:string "s" 1 1 nil) (:string "s" 1 1 2)
   ((:before "s" 1 nil nil) (:before "s" 1 nil 2))))

(deftest gf-key-no-keyword-dispatch.errors
  (list (progn (%gkn-f 'x "ab") (%gkn-f 'x "ab")
               (handler-case (%gkn-f 'x "ab" :bogus 1) (program-error () :pe)))
        (handler-case (%gkn-f 'x "ab" :start) (program-error () :pe))
        (handler-case (%gkn-f 'x) (program-error () :pe))
        (handler-case (%gkn-opt 'a 1 :d 2) (program-error () :pe)))
  (:pe :pe :pe :pe))

(defun %gkn-miss (s st en) (declare (ignore s st en)) nil)

(defun %gkn-bytes (fn n)
  (dotimes (i 100) (funcall fn))
  (min-allocated-bytes (lambda () (dotimes (i n) (funcall fn)))))

(deftest-emitting-only gf-key-no-keyword-dispatch.no-array
  (let ((g #'%gkn-miss))
    (< (%gkn-bytes (lambda () (%gkn-f g "abc")) 10000) 4000))
  t)
