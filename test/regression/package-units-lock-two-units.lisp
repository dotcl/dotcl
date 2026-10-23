;;; The dotcl-lock contrib with both versions of a system in units and none
;;; outside them: pulock2-x 1 in the unit PULOCK2-X@1 (with pulock2-a, its
;;; boundary), pulock2-x 2 in PULOCK2-X@2 (with pulock2-b). pulock2-app and
;;; pulock2-common stay global.

(require "asdf")
(require "dotcl-lock")

(defvar *pulock2-dir*
  (merge-pathnames "test/regression/package-units/lock-two-units/" (truename "./")))
(defvar *pulock2-translations*
  (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS")))
(defvar *pulock2-compiled* '())

(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:cl-source-file))
  (when (search (namestring *pulock2-dir*) (namestring (asdf:component-pathname c)))
    (push (asdf:component-name c) *pulock2-compiled*)))

(asdf:initialize-output-translations
 `(:output-translations
   (t (,(concatenate 'string (regression-temp-dir) "/pulock2-fasl/") :**/ :*.*.*))
   :ignore-inherited-configuration))
;;; A source registry over the fixture finds some pulock2-x outside the units
;;; too; the lock must still keep it out of the global namespace.
(asdf:initialize-source-registry
 `(:source-registry (:tree ,*pulock2-dir*) :inherit-configuration))
(dotcl-lock:use-lock (merge-pathnames "dotcl-systems.lock" *pulock2-dir*))

(defun pulock2-load ()
  (setf *pulock2-compiled* '())
  (asdf:load-system "pulock2-app")
  *pulock2-compiled*)

(defvar *pulock2-first-load* (pulock2-load))

(defun pulock2-relative (pathname)
  (enough-namestring pathname *pulock2-dir*))

(deftest package-units-two-units-run
  (funcall (read-from-string "pulock2-app:run"))
  ("1" "2"))

(deftest package-units-two-units-load-once
  (list (equal (sort (copy-list *pulock2-first-load*) #'string<)
               (if (emitting-mode-p) '("a" "app" "b" "common" "x" "x") '()))
        (pulock2-load)
        (symbol-value (find-symbol "*LOADS*" "PULOCK2-COMMON")))
  (t nil 1))

(deftest package-units-two-units-packages
  (flet ((in-unit (unit)
           (dotcl-lock:call-in-unit unit
             (lambda () (list (package-name (find-package "PULOCK2-X"))
                              (and (find-symbol "FAREWELL" "PULOCK2-X") t))))))
    (list (find-package "PULOCK2-X")
          (in-unit "pulock2-x@1")
          (in-unit "pulock2-x@2")
          (package-name (symbol-package (find-symbol "X-VERSION" "PULOCK2-A")))))
  (nil ("PULOCK2-X" nil) ("PULOCK2-X" t) "PULOCK2-A"))

(deftest package-units-two-units-registries
  (flet ((look ()
           (list (let ((s (asdf:find-system "pulock2-x" nil)))
                   (and s (pulock2-relative (asdf:system-source-directory s))))
                 (type-of (asdf:find-system "pulock2-a"))
                 (type-of (asdf:find-system "pulock2-b")))))
    (list (look)
          (dotcl-lock:call-in-unit "pulock2-x@1" #'look)
          (dotcl-lock:call-in-unit "pulock2-x@2" #'look)))
  ((nil dotcl-lock::proxy-system dotcl-lock::proxy-system)
   ("x-1/" asdf:system dotcl-lock::proxy-system)
   ("x-2/" dotcl-lock::proxy-system asdf:system)))

;;; From inside one unit, NIL is the way back out: no PULOCK2-X and no
;;; pulock2-x system there, and the global app is the real one.
(deftest package-units-two-units-back-to-global
  (dotcl-lock:call-in-unit "pulock2-x@1"
    (lambda ()
      (dotcl-lock:call-in-unit nil
        (lambda ()
          (list (find-package "PULOCK2-X")
                (asdf:find-system "pulock2-x" nil)
                (type-of (asdf:find-system "pulock2-app")))))))
  (nil nil asdf:system))

;;; A name looked up at run time follows the caller's unit, so from outside it
;;; finds nothing; CALL-IN-SYSTEM-UNIT runs it in the unit of the system.
(deftest package-units-two-units-call-in-system-unit
  (flet ((by-name (sym) (funcall (read-from-string sym))))
    (list (by-name "pulock2-a:x-version-by-name")
          (dotcl-lock:call-in-system-unit "pulock2-a"
            (lambda () (by-name "pulock2-a:x-version-by-name")))
          (dotcl-lock:call-in-system-unit "pulock2-b"
            (lambda () (by-name "pulock2-b:x-version-by-name")))
          (dotcl-lock:call-in-system-unit "pulock2-app"
            (lambda () (by-name "pulock2-a:x-version-by-name")))
          (mapcar #'dotcl-lock:system-unit '("pulock2-a" "pulock2-b" "pulock2-app" "pulock2-none"))
          (handler-case (dotcl-lock:system-unit "pulock2-x") (error () :ambiguous))))
  (nil "1" "2" nil ("PULOCK2-X@1" "PULOCK2-X@2" nil nil) :ambiguous))

(deftest package-units-two-units-fasls-apart
  (flet ((out (unit file)
           (dotcl-lock:call-in-unit unit
             (lambda () (pulock2-relative
                         (asdf:apply-output-translations (merge-pathnames file *pulock2-dir*)))))))
    (list (and (search "/unit-PULOCK2-X@1/" (out "pulock2-x@1" "x-1/x.lisp")) t)
          (and (search "/unit-PULOCK2-X@2/" (out "pulock2-x@2" "x-2/x.lisp")) t)
          (search "/unit-" (out nil "app/app.lisp"))))
  (t t nil))

(setf asdf:*system-definition-search-functions*
      (remove 'dotcl-lock::sysdef-lock-search asdf:*system-definition-search-functions*)
      dotcl-lock::*lock* nil
      (symbol-value (uiop:find-symbol* "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))
      *pulock2-translations*)
(asdf:clear-source-registry)
(remove-method #'asdf:perform
               (find-method #'asdf:perform '(:before)
                            (list (find-class 'asdf:compile-op) (find-class 'asdf:cl-source-file))))
