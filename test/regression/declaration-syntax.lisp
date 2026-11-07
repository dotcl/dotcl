;;; Malformed declaration specifiers and FLET / LABELS definitions are PROGRAM-ERRORs;
;;; TYPEP / SUBTYPEP reject a pathname, stream, readtable or structure as a type.

(defmacro %ds-err (form)
  `(handler-case (progn (eval ',form) :no-error) (error () :error)))

(deftest declaration-syntax.invalid
  (values (%ds-err (locally (declare (ignore . foo)) nil))
          (%ds-err (locally (declare (ignore 1)) nil))
          (%ds-err (locally (declare (ignorable "s")) nil))
          (%ds-err (locally (declare (dynamic-extent 1)) nil))
          (%ds-err (locally (declare (special . x)) nil))
          (%ds-err (locally (declare (special 1)) nil))
          (%ds-err (locally (declare (inline 1)) nil))
          (%ds-err (locally (declare (notinline . x)) nil))
          (%ds-err (locally (declare (ftype)) nil))
          (%ds-err (locally (declare (ftype (function () t) . foo)) nil))
          (%ds-err (locally (declare (optimize (speed 1 . 2))) nil))
          (%ds-err (flet ((foo))))
          (%ds-err (flet ((foo (&key ((x y . z))) nil))))
          (%ds-err (flet ((foo (&key ((x y z))) nil))))
          (%ds-err (let () (declare (ignore . foo)) nil)))
  :error :error :error :error :error :error :error :error :error :error :error :error :error :error :error)

(deftest declaration-syntax.valid
  (values (eval '(locally (declare (ignore x (function f))) 1))
          (eval '(locally (declare (special x y) (inline f (setf g)) (notinline h)) 2))
          (eval '(locally (declare (ftype (function () t) f) (ftype function g)) 3))
          (eval '(locally (declare (optimize speed (safety 1) (debug 0))) 4))
          (eval '(let ((x 1)) (declare (type integer x) (ignorable x) (optimize (speed 1))) x))
          (eval '(flet ((foo (&key ((:a b) 1) (c 2)) (list b c))) (foo)))
          (eval '(locally (declare (type integer)) 5)))
  1 2 3 4 1 (1 2) 5)

(deftest declaration-syntax.not-a-type
  (values (handler-case (typep 1 #p"x") (error () :error))
          (handler-case (typep 1 *readtable*) (error () :error))
          (handler-case (typep 1 *standard-output*) (error () :error))
          (handler-case (subtypep #p"x" t) (error () :error))
          (typep 1 'integer))
  :error :error :error :error t)
