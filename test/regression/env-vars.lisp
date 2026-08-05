;;; Regression tests for dotcl:setenv / dotcl:unsetenv (back uiop:getenv setf / unsetenv)

;;; set then get returns the value
(deftest env-setenv-get
  (progn
    (dotcl:setenv "DOTCL_REGRESSION_ENV" "value-1")
    (dotcl:getenv "DOTCL_REGRESSION_ENV"))
  "value-1")

;;; overwrite with a new value
(deftest env-setenv-overwrite
  (progn
    (dotcl:setenv "DOTCL_REGRESSION_ENV" "value-2")
    (dotcl:getenv "DOTCL_REGRESSION_ENV"))
  "value-2")

;;; unsetenv removes the variable -> getenv returns NIL
(deftest env-unsetenv
  (progn
    (dotcl:setenv "DOTCL_REGRESSION_ENV" "value-3")
    (dotcl:unsetenv "DOTCL_REGRESSION_ENV")
    (dotcl:getenv "DOTCL_REGRESSION_ENV"))
  nil)

;;; getenv on an absent variable returns NIL
(deftest env-getenv-absent
  (dotcl:getenv "DOTCL_REGRESSION_ABSENT_XYZ")
  nil)
