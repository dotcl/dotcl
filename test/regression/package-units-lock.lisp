;;; The dotcl-lock contrib: a lock file puts the second version of a system in
;;; a package unit, and ASDF loads both. pulock-app depends on pulock-a, which
;;; wants pulock-x 1, and on pulock-b, which wants pulock-x 2. The lock loads
;;; pulock-x 1 globally, pulock-x 2 and pulock-b in the unit PULOCK-X@2, and
;;; pulock-common, which both versions use, once. The second load of the
;;; tests below loads nothing again.

(require "asdf")
(require "dotcl-lock")

(defvar *pulock-dir*
  (merge-pathnames "test/regression/package-units/lock/" (truename "./")))
(defvar *pulock-translations*
  (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS")))
(defvar *pulock-compiled* '())
(defvar *pulock-mismatches* 0)

(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:cl-source-file))
  (when (search (namestring *pulock-dir*) (namestring (asdf:component-pathname c)))
    (push (asdf:component-name c) *pulock-compiled*)))

(asdf:initialize-output-translations
 `(:output-translations
   (t (,(concatenate 'string (regression-temp-dir) "/pulock-fasl/") :**/ :*.*.*))
   :ignore-inherited-configuration))
(dotcl-lock:use-lock (merge-pathnames "dotcl-systems.lock" *pulock-dir*))

(defun pulock-load ()
  (setf *pulock-compiled* '())
  (handler-bind ((dotcl:fasl-version-mismatch
                   (lambda (c) (declare (ignore c)) (incf *pulock-mismatches*))))
    (asdf:load-system "pulock-app"))
  *pulock-compiled*)

(defvar *pulock-first-load* (pulock-load))

(defun pulock-call (name &rest args)
  (apply (read-from-string name) args))

(defun pulock-relative (pathname)
  (enough-namestring pathname *pulock-dir*))

(deftest package-units-lock-two-versions
  (pulock-call "pulock-app:run")
  ("1" "2" (:hello "a") ((:hi "b") (:bye "b"))))

;;; Each file compiled once (nothing is compiled where LOAD-SYSTEM loads
;;; source), and nothing again on the second load.
(deftest package-units-lock-compiled-each-once
  (list (equal (sort (copy-list *pulock-first-load*) #'string<)
               (if (emitting-mode-p) '("a" "app" "b" "common" "x" "x") '()))
        (pulock-load)
        *pulock-mismatches*)
  (t nil 0))

(deftest package-units-lock-shared-dependency-once
  (list (symbol-value (find-symbol "*LOADS*" "PULOCK-COMMON"))
        (sort (copy-list (symbol-value (find-symbol "*NOTES*" "PULOCK-COMMON"))) #'string<))
  (1 ("x 1" "x 2")))

(deftest package-units-lock-packages
  (list (package-name (symbol-package (find-symbol "X-VERSION" "PULOCK-B")))
        (and (find-package "PULOCK-X@2!PULOCK-X") t)
        (find-symbol "FAREWELL" "PULOCK-X")
        (dotcl-lock:call-in-unit "PULOCK-X@2"
          (lambda () (and (find-symbol "FAREWELL" "PULOCK-X") t))))
  ("PULOCK-B" t nil t))

;;; Each side finds its own definition of the same name; the boundary system is
;;; a leaf outside the unit and the real system inside it.
(deftest package-units-lock-registries
  (flet ((look ()
           (list (pulock-relative (asdf:system-source-directory "pulock-x"))
                 (type-of (asdf:find-system "pulock-b"))
                 (eq (asdf:find-system "pulock-common")
                     (dotcl-lock:call-in-unit nil (lambda () (asdf:find-system "pulock-common")))))))
    (list (look) (dotcl-lock:call-in-unit "PULOCK-X@2" #'look)))
  (("x-1/" dotcl-lock::proxy-system t)
   ("x-2/" asdf:system nil)))

(deftest package-units-lock-fasls-apart
  (flet ((out (file) (pulock-relative
                      (asdf:apply-output-translations (merge-pathnames file *pulock-dir*)))))
    (list (search "/unit-" (out "x-1/x.lisp"))
          (and (search "/unit-PULOCK-X@2/" (dotcl-lock:call-in-unit "PULOCK-X@2"
                                             (lambda () (out "x-2/x.lisp"))))
               t)))
  (nil t))

;;; The lock spells the unit in mixed case; the unit is named in upper case.
(deftest package-units-lock-unit-name-upcased
  (list (dotcl-lock:call-in-unit "pulock-x@2" (lambda () (funcall (read-from-string "pulock-x:version"))))
        (and (find-package "PULOCK-X@2!PULOCK-X") t))
  ("2" t))

(deftest package-units-lock-not-in-lock-warns-once
  (let ((names '()))
    (handler-bind ((dotcl-lock:system-not-in-lock
                     (lambda (c)
                       (push (dotcl-lock:system-not-in-lock-name c) names)
                       (muffle-warning c))))
      (list (asdf:find-system "pulock-not-in-lock" nil)
            (asdf:find-system "pulock-not-in-lock" nil)
            names)))
  (nil nil ("pulock-not-in-lock")))

(setf asdf:*system-definition-search-functions*
      (remove 'dotcl-lock::sysdef-lock-search asdf:*system-definition-search-functions*)
      dotcl-lock::*lock* nil
      (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))
      *pulock-translations*)
(remove-method #'asdf:perform
               (find-method #'asdf:perform '(:before)
                            (list (find-class 'asdf:compile-op) (find-class 'asdf:cl-source-file))))
