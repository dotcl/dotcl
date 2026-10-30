;;; TYPECASE over structure names leaves a Symbol on the stack at the
;;; :INCLUDE-parent loop join. Widening it to LispObject is not verifiable.

(setf dotcl:*save-sil* t)

(defstruct stj-a a)
(defstruct stj-b b)

(defun %stj-run (x)
  (typecase x
    (stj-a :a)
    (stj-b :b)
    (t :other)))

(defun %stj-symbol-join-p (sil)
  (and (search "(CALL Symbol.get_StructParent) (CASTCLASS Symbol)" sil)
       (not (search "(CASTCLASS LispObject)" sil))
       t))

(deftest struct-typep-join-symbol.values
  (list (%stj-run (make-stj-a)) (%stj-run (make-stj-b)) (%stj-run 42))
  (:a :b :other))

(deftest-emitting-only struct-typep-join-symbol.sil
  (%stj-symbol-join-p (%flat-text (dotcl:function-sil #'%stj-run)))
  t)

(defun %stj-file-text (path)
  (with-open-file (in path :direction :input)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil) while line
            do (write-line line out)))))

(defun %stj-file-symbol-join-p (path)
  (let ((sil (%stj-file-text path)))
    (and (search "(:CALL \"Symbol.get_StructParent\") (:CASTCLASS \"Symbol\")" sil)
         (not (search "(:CASTCLASS \"LispObject\")" sil))
         t)))

(defun %stj-compile-file-check ()
  (let* ((src (regression-temp-file "stj.lisp"))
         (fasl (regression-temp-file "stj.fasl"))
         (sil (regression-temp-file "stj.sil")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defstruct stj-fasl-a a)" s)
      (write-line "(defstruct stj-fasl-b b)" s)
      (write-line "(defun stj-fasl-run (x)" s)
      (write-line "  (typecase x" s)
      (write-line "    (stj-fasl-a :a)" s)
      (write-line "    (stj-fasl-b :b)" s)
      (write-line "    (t :other)))" s))
    (load (compile-file src :output-file fasl :sil t))
    (list (stj-fasl-run (make-stj-fasl-a))
          (stj-fasl-run (make-stj-fasl-b))
          (stj-fasl-run 1)
          (%stj-file-symbol-join-p sil))))

(deftest-emitting-only struct-typep-join-symbol.compile-file
  (%stj-compile-file-check)
  (:a :b :other t))
