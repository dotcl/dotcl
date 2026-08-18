;;; A print-object method on a parent struct must be used for structs that
;;; (:include ...) it.
;;;
;;; The printer asked "is there a print-object method specialized on exactly this
;;; struct's class", so a method on the parent was invisible and the sub-struct
;;; printed as #S(...) while the parent printed through the method. (:include ...)
;;; puts the parent in the class precedence list, and CL dispatches print-object
;;; like any other generic function, so the parent's method applies.
;;;
;;; quri is the case that surfaced it: print-object is defined on QURI.URI:URI and
;;; the values handed back are sub-structs like URI-HTTP, so every printed URI came
;;; out as #S(URI-HTTP :SCHEME ...) instead of the URL.

(defstruct pos-base a)
(defstruct (pos-derived (:include pos-base)) b)
(defstruct (pos-deeper (:include pos-derived)) c)
(defstruct pos-unrelated z)

(defmethod print-object ((x pos-base) stream)
  (format stream "#<POS-BASE a=~a>" (pos-base-a x)))

(deftest print-object-struct-include.parent
  (format nil "~a" (make-pos-base :a 1))
  "#<POS-BASE a=1>")

(deftest print-object-struct-include.child
  (format nil "~a" (make-pos-derived :a 1 :b 2))
  "#<POS-BASE a=1>")

(deftest print-object-struct-include.grandchild
  (format nil "~a" (make-pos-deeper :a 1 :b 2 :c 3))
  "#<POS-BASE a=1>")

;;; ~s takes the same path.
(deftest print-object-struct-include.escape
  (format nil "~s" (make-pos-derived :a 1 :b 2))
  "#<POS-BASE a=1>")

;;; princ-to-string is the shape libraries hit when they splice a value into a
;;; message, and it was printing the whole #S(...) there.
(deftest print-object-struct-include.princ-to-string
  (concatenate 'string "v is " (princ-to-string (make-pos-derived :a 7 :b 8)))
  "v is #<POS-BASE a=7>")

;;; A struct with no applicable method still gets the default printer.
(deftest print-object-struct-include.unrelated
  (format nil "~a" (make-pos-unrelated :z 9))
  "#S(POS-UNRELATED :Z 9)")

;;; The gate only asks whether SOME method applies; which one runs is ordinary
;;; generic-function dispatch, so a method on the child must still win.
(defmethod print-object ((x pos-derived) stream)
  (format stream "#<POS-DERIVED b=~a>" (pos-derived-b x)))

(deftest print-object-struct-include.most-specific-wins
  (list (format nil "~a" (make-pos-base :a 1))
        (format nil "~a" (make-pos-derived :a 1 :b 2))
        (format nil "~a" (make-pos-deeper :a 1 :b 2 :c 3)))
  ("#<POS-BASE a=1>" "#<POS-DERIVED b=2>" "#<POS-DERIVED b=2>"))
