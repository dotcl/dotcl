;;; DEFGENERIC with :GENERIC-FUNCTION-CLASS passes the name to MAKE-INSTANCE
;;; as the :NAME initarg (AMOP), so GENERIC-FUNCTION-NAME answers it instead of
;;; leaving the generic function unnamed.

(defclass gcon-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defgeneric gcon-hh (x) (:generic-function-class gcon-gf))
(defmethod gcon-hh ((x integer)) (* x 2))

(deftest gf-class-option.name
  (list (dotcl-mop:generic-function-name #'gcon-hh)
        (gcon-hh 4)
        (class-name (class-of #'gcon-hh)))
  (gcon-hh 8 gcon-gf))

;; A user initialize-instance method sees the name among the initargs.
(defvar *gcon-seen-name* nil)
(defclass gcon-gf2 (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defmethod initialize-instance :after ((gf gcon-gf2) &key name &allow-other-keys)
  (setf *gcon-seen-name* name))
(defgeneric gcon-ii (x) (:generic-function-class gcon-gf2))

(deftest gf-class-option.initarg-seen
  *gcon-seen-name*
  gcon-ii)

;; A (SETF name) generic function of a user class is named, not left unnamed.
(defgeneric (setf gcon-acc) (v x) (:generic-function-class gcon-gf))

(deftest gf-class-option.setf-name
  (let ((n (dotcl-mop:generic-function-name #'(setf gcon-acc))))
    (and n (not (and (symbolp n) (string= (symbol-name n) "UNNAMED")))))
  t)
