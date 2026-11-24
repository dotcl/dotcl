;;; (TYPEP string '(SIMPLE-ARRAY CHARACTER (*))) and its sized forms answer from the cached test, not by
;;; interpreting the specifier each time. The answers must stay those of the general TYPEP.

(defun %tss-spec-answers (spec)
  (let ((fn (compile nil `(lambda (x) (typep x ',spec))))
        (objs (list "" "abc" (make-string 3) (make-array 3 :element-type 'character)
                    (make-array 3 :element-type 'character :adjustable t)
                    (make-array 3 :element-type 'character :fill-pointer 2)
                    (coerce "abc" 'simple-base-string) #(1 2 3) (vector) 1 nil 'abc)))
    (loop for o in objs
          collect (eq (and (funcall fn o) t) (and (typep o (identity spec)) t)))))

(deftest-compiled-only typep-simple-array-strings.char
  (every #'identity (%tss-spec-answers '(simple-array character (*))))
  t)

(deftest-compiled-only typep-simple-array-strings.base-char
  (every #'identity (%tss-spec-answers '(simple-array base-char (*))))
  t)

(deftest-compiled-only typep-simple-array-strings.sized
  (every #'identity (%tss-spec-answers '(simple-array character (3))))
  t)

(deftest-compiled-only typep-simple-array-strings.values
  (let ((f (compile nil '(lambda (x) (typep x '(simple-array character (3)))))))
    (list (and (funcall f "abc") t) (and (funcall f "ab") t) (and (funcall f (make-string 3)) t)
          (and (funcall f (make-array 3 :element-type 'character :adjustable t)) t)))
  (t nil t nil))
