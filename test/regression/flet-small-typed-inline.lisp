;;; A small FLET function whose body declares only types (or IGNORE) is substituted
;;; at its calls like one without declarations; the substituted call still checks
;;; the declared types.

(setf dotcl:*save-sil* t)

(defun %fsti-hex (string)
  (let ((key (make-array (floor (length string) 2) :element-type '(unsigned-byte 8))))
    (flet ((digit (char)
             (declare (type base-char char))
             (let ((x (position char "0123456789abcdef" :test #'char-equal)))
               (or x (error "bad hex ~a" string)))))
      (loop for i from 0
            for j from 0 below (length string) by 2
            do (setf (aref key i) (+ (* (digit (char string j)) 16)
                                     (digit (char string (1+ j))))))
      key)))

(defun %fsti-typed (x)
  (flet ((f (n) (declare (fixnum n) (ignorable n)) (list n)))
    (f x)))

(defun %fsti-special (x)
  (flet ((f (n) (declare (special n)) (list n)))
    (f x)))

(defun %fsti-has-function-p (fn)
  (and (search "MAKE-FUNCTION" (%flat-text (dotcl:function-sil fn))) t))

(deftest flet-small-typed-inline.values
  (list (coerce (%fsti-hex "00FFa5") 'list)
        (%fsti-typed 7)
        (handler-case (%fsti-typed :x) (type-error (e) (list :type-error (type-error-datum e))))
        (handler-case (%fsti-hex "0G") (error () :error))
        (%fsti-special 3))
  ((0 255 165) (7) (:type-error :x) :error (3)))

(deftest-compiled-only flet-small-typed-inline.substituted
  (list (%fsti-has-function-p #'%fsti-hex)
        (%fsti-has-function-p #'%fsti-typed)
        (%fsti-has-function-p #'%fsti-special))
  (nil nil t))
