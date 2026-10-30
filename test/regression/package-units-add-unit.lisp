;;; dotcl-lock:add-unit without a lock file: puadd-x 1 is found globally
;;; through the source registry, puadd-x 2 is put in the unit PUADD-X@2 by its
;;; .asd alone, and puadd-common, which both use, stays global.

(require "asdf")
(require "dotcl-lock")

(defvar *puadd-dir*
  (merge-pathnames "test/regression/package-units/add-unit/" (truename "./")))
(defvar *puadd-translations*
  (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS")))

(asdf:initialize-output-translations
 `(:output-translations
   (t (,(concatenate 'string (regression-temp-dir) "/puadd-fasl/") :**/ :*.*.*))
   :ignore-inherited-configuration))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(merge-pathnames "global/" *puadd-dir*)) :inherit-configuration))
(setf dotcl-lock::*lock* nil)

(defvar *puadd-global-before* (progn (asdf:load-system "puadd-x")
                                     (uiop:symbol-call :puadd-x :version)))
(defvar *puadd-unit*
  (dotcl-lock:add-unit "puadd-x@2" (merge-pathnames "x-2/puadd-x.asd" *puadd-dir*)))

(defun puadd-in-unit (thunk) (dotcl-lock:call-in-unit "puadd-x@2" thunk))

(defvar *puadd-compiled* '())
(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:cl-source-file))
  (when (search (namestring *puadd-dir*) (namestring (asdf:component-pathname c)))
    (push (asdf:component-name c) *puadd-compiled*)))

(defun puadd-load-in-unit ()
  (setf *puadd-compiled* '())
  (list (puadd-in-unit (lambda ()
                         (asdf:load-system "puadd-x")
                         (uiop:symbol-call :puadd-x :version)))
        (sort (copy-list *puadd-compiled*) #'string<)))

(deftest package-units-add-unit-infers
  (let ((u (dotcl-lock:find-unit "puadd-x@2")))
    (list *puadd-unit*
          (dotcl-lock::unit-packages u)
          (dotcl-lock::unit-systems u)
          (dotcl-lock:system-unit "puadd-x")))
  ("PUADD-X@2" ("PUADD-X" "PUADD-X-MODULE" "PUADD-X-SCRATCH" "PUADD-X-TWO" "PUADD-X/UTIL" "PUADD-XN")
   ("puadd-x") "PUADD-X@2"))

;;; puadd-common, loaded globally before the unit was made, is neither
;;; compiled nor loaded again for the unit.
(deftest package-units-add-unit-both-versions
  (let ((first (puadd-load-in-unit)))
    (list *puadd-global-before*
          (first first)
          (equal (second first) (if (emitting-mode-p) '("util" "x") '()))
          (puadd-load-in-unit)
          (uiop:symbol-call :puadd-x :version)
          (find-package "PUADD-X/UTIL")
          (symbol-value (find-symbol "*LOADS*" "PUADD-COMMON"))))
  ("1" ("2" "2") t (("2" "2") ()) "1" nil 1))

(deftest package-units-add-unit-registries
  (flet ((look ()
           (enough-namestring (asdf:system-source-directory (asdf:find-system "puadd-x"))
                              *puadd-dir*)))
    (list (look) (puadd-in-unit #'look)))
  ("global/x-1/" "x-2/"))

;;; The global puadd-x 1 has the nickname PUADD-XN too: the unit owns it, so
;;; defining puadd-x 2 did not collide, and its module stays in the unit.
(deftest package-units-add-unit-nickname-and-module
  (flet ((look ()
           (list (package-name (find-package "PUADD-XN"))
                 (funcall (find-symbol "VERSION" "PUADD-XN"))
                 (and (member "PUADD-X-MODULE" *modules* :test #'string=) t))))
    (list (look) (puadd-in-unit #'look)))
  (("PUADD-X" "1" nil) ("PUADD-X" ("2" "2") t)))

;;; A package the unit's code makes at run time is the unit's when its name is
;;; one ADD-UNIT found.
(deftest package-units-add-unit-runtime-package
  (let ((p (puadd-in-unit (lambda () (uiop:symbol-call :puadd-x :scratch-package)))))
    (list (puadd-in-unit (lambda () (package-name p)))
          (package-name p)
          (find-package "PUADD-X-SCRATCH")))
  ("PUADD-X-SCRATCH" "PUADD-X@2!PUADD-X-SCRATCH" nil))

;;; Without a lock file, systems outside the unit are not expected to be in
;;; one, so they load with no warning.
(deftest package-units-add-unit-no-lock-warning
  (let ((warnings 0))
    (handler-bind ((dotcl-lock:system-not-in-lock
                     (lambda (c) (incf warnings) (muffle-warning c))))
      (asdf:load-system "puadd-y"))
    warnings)
  0)

(deftest package-units-add-unit-errors
  (flet ((err (thunk) (handler-case (progn (funcall thunk) :no-error) (error () :error))))
    (list (err (lambda () (dotcl-lock:add-unit "PUADD-X@2" (merge-pathnames "x-2/puadd-x.asd" *puadd-dir*))))
          (err (lambda () (dotcl-lock:add-unit "puadd-none" (merge-pathnames "none/none.asd" *puadd-dir*))))
          (err (lambda () (dotcl-lock:use-lock "dotcl-systems.lock")))))
  (:error :error :error))

(setf asdf:*system-definition-search-functions*
      (remove 'dotcl-lock::sysdef-lock-search asdf:*system-definition-search-functions*)
      dotcl-lock::*lock* nil
      (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))
      *puadd-translations*)
(asdf:clear-source-registry)
(remove-method #'asdf:perform
               (find-method #'asdf:perform '(:before)
                            (list (find-class 'asdf:compile-op) (find-class 'asdf:cl-source-file))))
