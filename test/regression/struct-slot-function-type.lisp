;;; A structure slot declared with a type TYPEP cannot decide.
;;;
;;; A slot's :TYPE is checked when the slot is stored. The check used TYPEP on
;;; the declared type as written, and TYPEP of a FUNCTION compound type is an
;;; error (CLHS: TYPEP does not accept (FUNCTION ...) or (VALUES ...)). So a
;;; perfectly ordinary declaration like
;;;
;;;   (test (lambda (a) nil) :type (function (string) boolean))
;;;
;;; made every store to the slot signal "FUNCTION compound type specifier not
;;; supported in TYPEP", and a library that set such a slot failed to load.
;;;
;;; The check now uses a supertype TYPEP accepts: (FUNCTION ...) is checked as
;;; FUNCTION, through OR / AND and through DEFTYPE. A non-function is still
;;; refused.

(defstruct ssft-dispatcher
  (test (lambda (a) (declare (ignore a)) nil) :type (function (string) boolean))
  (maybe nil :type (or null (function (t) t)))
  (plain 0 :type fixnum))

(deftest struct-slot-function-type.store-function
  (let ((d (make-ssft-dispatcher)))
    (setf (ssft-dispatcher-test d) (lambda (s) (plusp (length s))))
    (funcall (ssft-dispatcher-test d) "abc"))
  t)

(deftest struct-slot-function-type.store-inside-or
  (let ((d (make-ssft-dispatcher)))
    (setf (ssft-dispatcher-maybe d) #'identity)
    (list (funcall (ssft-dispatcher-maybe d) 7)
          (progn (setf (ssft-dispatcher-maybe d) nil) (ssft-dispatcher-maybe d))))
  (7 nil))

;; The weakened check still refuses what cannot be a function.
(deftest struct-slot-function-type.non-function-refused
  (let ((d (make-ssft-dispatcher)))
    (list (handler-case (progn (setf (ssft-dispatcher-test d) "not a function") :stored)
            (type-error () :type-error))
          (handler-case (progn (setf (ssft-dispatcher-maybe d) 42) :stored)
            (type-error () :type-error))))
  (:type-error :type-error))

;; Other slots are checked exactly as before.
(deftest struct-slot-function-type.other-slots-unchanged
  (let ((d (make-ssft-dispatcher)))
    (handler-case (progn (setf (ssft-dispatcher-plain d) "x") :stored)
      (type-error () :type-error)))
  :type-error)

;; Through a DEFTYPE: the declared name expands to a FUNCTION compound.
(deftype ssft-callback () '(function (t) t))

(defstruct ssft-holder
  (cb #'identity :type ssft-callback))

(deftest struct-slot-function-type.through-deftype
  (let ((h (make-ssft-holder)))
    (setf (ssft-holder-cb h) #'1+)
    (list (funcall (ssft-holder-cb h) 1)
          (handler-case (progn (setf (ssft-holder-cb h) 3) :stored)
            (type-error () :type-error))))
  (2 :type-error))
