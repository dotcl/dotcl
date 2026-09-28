;;; DESTRUCTURING-BIND must reject a malformed keyword portion.
;;;
;;; The part of the argument list that &KEY consumes has to be a proper list of
;;; even length whose keys are symbols (CLHS 3.4.1.4.1, 3.5.1.6). dotcl looked
;;; each key up with MEMBER and never checked the shape, so a list whose pairs
;;; were out of step bound every key to its default and the mistake travelled on
;;; as a wrong value.
;;;
;;; The shape that found this comes from a system definition. A component form
;;; written as
;;;
;;;   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"
;;;    :depends-on ("pkgdcl" "conditions"))
;;;
;;; reads on a host without :UNIX as
;;;
;;;   (:file "ffi-functions" :pathname :depends-on ("pkgdcl" "conditions"))
;;;
;;; and the parser that destructures it saw :PATHNAME paired with :DEPENDS-ON
;;; and the dependency list standing alone. Silently binding :PATHNAME to
;;; :DEPENDS-ON pushed the report all the way down to MAKE-INSTANCE, which
;;; complained that ("pkgdcl" "conditions") is not a valid initarg key: a true
;;; statement about a value that was never meant to be an initarg key at all.
;;; Reporting the malformed list where it is consumed names the actual defect,
;;; which is what other implementations do here.

(defun %dbks (thunk)
  (handler-case (funcall thunk)
    (program-error () :program-error)
    (error (e) (list :other (type-of e)))))

;;; --- malformed keyword portions are rejected

(deftest destructuring-bind-key-shape.odd-tail
  (%dbks (lambda () (destructuring-bind (a &key b c) '(1 :b 2 :c) (list a b c))))
  :program-error)

;;; &ALLOW-OTHER-KEYS permits unrecognized keys, not an unpaired list.
(deftest destructuring-bind-key-shape.odd-tail-allow-other-keys
  (%dbks (lambda ()
           (destructuring-bind (a &rest r &key b &allow-other-keys) '(1 :b 2 :c)
             (list a b r))))
  :program-error)

(deftest destructuring-bind-key-shape.non-symbol-key
  (%dbks (lambda () (destructuring-bind (a &key b) '(1 "x" 2) (list a b))))
  :program-error)

;;; A dotted tail is not a keyword list either.
(deftest destructuring-bind-key-shape.dotted-tail
  (%dbks (lambda () (destructuring-bind (a &key b) '(1 . 2) (list a b))))
  :program-error)

;;; The keyword portion begins after the optionals, so what is checked must be
;;; what is left at that point rather than the whole argument list.
(deftest destructuring-bind-key-shape.odd-tail-after-optional
  (%dbks (lambda ()
           (destructuring-bind (a &optional o &key b) '(1 9 :b) (list a o b))))
  :program-error)

;;; The component form above, destructured the way a system definition parser
;;; destructures it. No system definition facility is needed to state it.
(deftest destructuring-bind-key-shape.component-form-shape
  (%dbks (lambda ()
           (destructuring-bind (type name &key pathname depends-on)
               '(:file "ffi-functions" :pathname :depends-on ("pkgdcl" "conditions"))
             (list type name pathname depends-on))))
  :program-error)

;;; A macro lambda list destructures the same way.
(deftest destructuring-bind-key-shape.macro-lambda-list
  (%dbks (lambda ()
           (eval '(progn (defmacro dbks-m (a &key b) (list 'quote (list a b)))
                         (macroexpand-1 '(dbks-m 1 :b))))))
  :program-error)

;;; --- well-formed keyword portions are untouched

(deftest destructuring-bind-key-shape.plain
  (destructuring-bind (a &key b c) '(1 :c 3 :b 2) (list a b c))
  (1 2 3))

(deftest destructuring-bind-key-shape.empty-tail
  (destructuring-bind (a &key (b 7)) '(1) (list a b))
  (1 7))

(deftest destructuring-bind-key-shape.supplied-p
  (destructuring-bind (a &key (b 7 bp)) '(1 :b 2) (list a b bp))
  (1 2 t))

(deftest destructuring-bind-key-shape.rest-and-key
  (destructuring-bind (a &rest r &key b) '(1 :b 2) (list a r b))
  (1 (:b 2) 2))

(deftest destructuring-bind-key-shape.unrecognized-key-still-accepted
  (destructuring-bind (a &key b) '(1 :b 2 :zzz 3) (list a b))
  (1 2))

(deftest destructuring-bind-key-shape.aux-after-key
  (destructuring-bind (a &key b &aux (c 5)) '(1 :b 2) (list a b c))
  (1 2 5))

(deftest destructuring-bind-key-shape.explicit-keyword-name
  (destructuring-bind (a &key ((:from-here b) 0)) '(1 :from-here 4) (list a b))
  (1 4))
