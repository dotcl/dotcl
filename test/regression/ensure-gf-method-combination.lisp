;;; ENSURE-GENERIC-FUNCTION with :METHOD-COMBINATION gives the generic function
;;; that combination (CLHS ensure-generic-function), for a new function and for an
;;; existing one.

(defgeneric egmc-source (x) (:method-combination list))
(defgeneric egmc-source-last (x) (:method-combination progn :most-specific-last))

(deftest ensure-gf-method-combination.new-function
  (progn
    (ensure-generic-function
     'egmc-target
     :lambda-list '(x)
     :method-combination (dotcl-mop:find-method-combination #'egmc-source 'list nil))
    (eval '(defmethod egmc-target list ((x integer)) :a))
    (eval '(defmethod egmc-target list ((x number)) :b))
    (egmc-target 1))
  (:a :b))

(deftest ensure-gf-method-combination.existing-function
  (progn
    (eval '(defgeneric egmc-existing (x)))
    (ensure-generic-function 'egmc-existing
                             :method-combination (dotcl-mop:find-method-combination #'egmc-source 'list nil))
    (eval '(defmethod egmc-existing list ((x integer)) :a))
    (egmc-existing 1))
  (:a))

(deftest ensure-gf-method-combination.back-to-standard
  (progn
    (eval '(defgeneric egmc-back (x) (:method-combination list)))
    (ensure-generic-function 'egmc-back
                             :method-combination (dotcl-mop:find-method-combination #'egmc-existing 'standard nil))
    (eval '(defmethod egmc-back ((x integer)) :std))
    (egmc-back 1))
  :std)
