;;; A symbol token has one package marker or two adjacent ones; another unescaped marker after
;;; the name started is a READER-ERROR (CLHS 2.3.5, as in SBCL). :::love used to read as a
;;; symbol named ":", cl:::car as COMMON-LISP::, and so on.

(defun %rtmc-kind (string)
  (handler-case (progn (read-from-string string) :read)
    (reader-error () :reader-error)
    (error () :other-error)))

(deftest reader-too-many-colons
  (mapcar #'%rtmc-kind
          '(":::love" "cl:::car" "cl::car::x" "a:b:c" ":a:b" "::love" "cl:car" "cl::car" ":love" ))
  (:reader-error :reader-error :reader-error :reader-error :reader-error
   :read :read :read :read))
