;;; DEFSTRUCT option combinations that are errors, as in SBCL: more than one :INCLUDE,
;;; :INITIAL-OFFSET without :TYPE, a print option on a :TYPE structure, and a :TYPE other
;;; than LIST, VECTOR or (VECTOR element-type). The valid neighbours are unchanged.

(defun %doc (form)
  (handler-case (progn (eval form) :ok) (error () :error)))

(defstruct doc-p1 a)
(defstruct doc-p2 b)

(deftest defstruct-option-combinations.errors
  (list (%doc '(defstruct (doc-a (:include doc-p1) (:include doc-p2)) c))
        (%doc '(defstruct (doc-b (:initial-offset 2)) a))
        (%doc '(defstruct (doc-c (:type list) (:print-function doc-pf)) a))
        (%doc '(defstruct (doc-d (:type list) (:print-object doc-pf)) a))
        (%doc '(defstruct (doc-e (:type array)) a))
        (%doc '(defstruct (doc-f (:type simple-vector)) a)))
  (:error :error :error :error :error :error))

(deftest defstruct-option-combinations.valid-neighbours
  (list (%doc '(defstruct (doc-g (:include doc-p1)) c))
        (%doc '(defstruct (doc-h (:type list) (:initial-offset 2)) a))
        (%doc '(defstruct (doc-i (:type vector) :named (:initial-offset 1)) a))
        (%doc '(defstruct (doc-j (:print-function doc-pf)) a))
        (%doc '(defstruct (doc-k (:type (vector t))) a))
        (%doc '(defstruct (doc-l (:type list) (:conc-name l-) (:copier nil) (:predicate nil)) a)))
  (:ok :ok :ok :ok :ok :ok))
