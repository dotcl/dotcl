;;; Reader-macro setup rejects what can never work: a digit as a dispatch sub-character
;;; (SIMPLE-ERROR), a non-function as the reader function (TYPE-ERROR), a radix outside
;;; 2..36 in #R (READER-ERROR). Valid setups keep working.

(defmacro %rmac-class (form)
  `(handler-case (progn ,form :ok)
     (type-error () :type-error)
     (reader-error () :reader-error)
     (error () :error)))

(deftest reader-macro-argument-checks.radix
  (list (%rmac-class (read-from-string "#37r1"))
        (%rmac-class (read-from-string "#1r1"))
        (read-from-string "#36rZ") (read-from-string "#2r101") (read-from-string "#16rFF"))
  (:reader-error :reader-error 35 5 255))

(deftest reader-macro-argument-checks.set-macro-character
  (let ((*readtable* (copy-readtable nil)))
    (list (%rmac-class (set-macro-character #\@ 42))
          (%rmac-class (set-macro-character #\@ nil))
          (%rmac-class (set-macro-character #\@ (lambda (s c) (declare (ignore s c)) 1)))
          (%rmac-class (set-macro-character #\@ 'car))))
  (:type-error :type-error :ok :ok))

(deftest reader-macro-argument-checks.set-dispatch-macro-character
  (let ((*readtable* (copy-readtable nil)))
    (list (%rmac-class (set-dispatch-macro-character #\# #\1 (lambda (s c n) (declare (ignore s c n)) nil)))
          (%rmac-class (set-dispatch-macro-character #\# #\$ 42))
          (%rmac-class (set-dispatch-macro-character #\# #\$ (lambda (s c n) (declare (ignore s c n)) :dollar)))
          (read-from-string "#$x")))
  (:error :type-error :ok :dollar))
