;;; A class (or condition) with a :READER, defined in a package that is then
;;; deleted, does not take the place of a same-named reader defined later in
;;; another package. The generic function lookup fell back to any registered
;;; generic function of the same name whose symbol had no home package, which
;;; is what the deleted package's symbols become, so DEFCLASS in the new
;;; package took the old generic function and the new reader stayed
;;; undefined. Coalton's tests define the same exception type in a fresh
;;; package per test and delete the package after.

(defun %gfndp-define (pkg-name)
  (let ((*package* (make-package pkg-name :use '(:cl))))
    (eval (read-from-string
           "(progn (defclass gfndp-c () ((a :initarg :a :reader gfndp-c-a)))
                   (define-condition gfndp-e (error) ((b :initarg :b :reader gfndp-e-b))))"))
    (prog1 (list (funcall (read-from-string "gfndp-c-a")
                          (make-instance (read-from-string "gfndp-c") :a 1))
                 (funcall (read-from-string "gfndp-e-b")
                          (make-condition (read-from-string "gfndp-e") :b 2)))
      (delete-package *package*))))

(deftest gf-name-after-delete-package.readers
  (list (%gfndp-define "GFNDP-ONE") (%gfndp-define "GFNDP-TWO"))
  ((1 2) (1 2)))
