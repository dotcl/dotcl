;;; (:nuget "Package.Id" ...) as a component in a system definition.
;;;
;;; A system declares the NuGet packages it needs instead of calling
;;; NUGET:REQUIRE from inside its own code. What is pinned here is the
;;; declaration being parsed into a component that carries the right values, and
;;; the mapping from those values to NUGET:REQUIRE's keywords.
;;;
;;; Resolving a package is not exercised: it wants the network and the .NET SDK.
;;; The end-to-end path (ASDF:LOAD-SYSTEM running the component) is checked by
;;; hand rather than here, because defining a system inside a file this suite
;;; LOADs makes ASDF treat that file as the system's definition and try to read it
;;; again.

(require "dotcl-nuget-asdf")

(defparameter *nac-system*
  (asdf:defsystem "nac-basic"
  :serial t
  :components ((:nuget "Newtonsoft.Json" :nuget-version "13.*")
               (:nuget "SkiaSharp" :nuget-version "2.88.7" :rid "win-arm64" :prerelease t)
               (:nuget "Plain.Package"))))

(defun %nac (name)
  (find name (asdf:component-children *nac-system*)
        :key #'asdf:component-name :test #'string=))

;;; The component type resolves. ASDF looks a type up in its own package, so this
;;; is the registration working, not just the class existing.
(deftest nuget-asdf.component-type-is-registered
  (let ((class (find-class 'asdf::nuget nil)))
    (list (notnot class)
          (notnot (eq class (find-class 'dotcl-nuget-asdf:nuget-package)))))
  (t t))

(deftest nuget-asdf.declaration-is-parsed
  (let ((c (%nac "SkiaSharp")))
    (list (asdf:component-name c)
          (dotcl-nuget-asdf:nuget-version c)
          (dotcl-nuget-asdf:nuget-rid c)
          (notnot (dotcl-nuget-asdf:nuget-prerelease c))
          (dotcl-nuget-asdf:nuget-tfm c)))
  ("SkiaSharp" "2.88.7" "win-arm64" t nil))

;;; A floating spec has to survive parsing. This is the case ASDF's own :VERSION
;;; silently drops, which is the reason for a keyword of our own.
(deftest nuget-asdf.floating-version-survives
  (dotcl-nuget-asdf:nuget-version (%nac "Newtonsoft.Json"))
  "13.*")

(deftest nuget-asdf.no-keywords-is-all-defaults
  (let ((c (%nac "Plain.Package")))
    (list (dotcl-nuget-asdf:nuget-version c)
          (dotcl-nuget-asdf:nuget-source c)
          (dotcl-nuget-asdf:nuget-rid c)))
  (nil nil nil))

;;; What reaches NUGET:REQUIRE: the keywords that were written and no others, so
;;; the defaults in NUGET:RESOLVE apply to the rest.
(deftest nuget-asdf.require-arguments
  (list (dotcl-nuget-asdf::%require-arguments (%nac "Plain.Package"))
        (dotcl-nuget-asdf::%require-arguments (%nac "Newtonsoft.Json"))
        (dotcl-nuget-asdf::%require-arguments (%nac "SkiaSharp")))
  (nil
   (:version "13.*")
   (:version "2.88.7" :prerelease t :rid "win-arm64")))

;;; :VERSION is ASDF's own component version and DEFSYSTEM takes it before the
;;; component is built, so it never reaches NuGet. An exact version even parses,
;;; which would make the mistake work in the easy case and fail silently in the
;;; floating one -- hence the separate keyword, and the refusal in PERFORM.
(defparameter *nac-misuse*
  (asdf:defsystem "nac-misuse"
    :components ((:nuget "Bad.Package" :version "2.0.0"))))

(deftest nuget-asdf.version-keyword-does-not-reach-nuget
  (let ((c (first (asdf:component-children *nac-misuse*))))
    (list (dotcl-nuget-asdf:nuget-version c)
          (notnot (asdf:component-version c))))
  (nil t))


;;; --- Carrying the declaration into a built artifact ------------------------
;;;
;;; `dotcl build` and `dotcl pack` do not LOAD the system, they concatenate its
;;; sources and compile the result -- and ASDF's concatenation gathers files. A
;;; (:nuget ...) component is not a file, so the declaration was dropped, in the
;;; one direction where it matters most: the built artifact is what runs on a
;;; machine with no .NET SDK. The build turns the declarations back into source
;;; and puts them in front of the system's own code.

(deftest nuget-asdf.components-of-a-system
  (mapcar #'asdf:component-name
          (dotcl-nuget-asdf:system-nuget-components *nac-system*))
  ("Newtonsoft.Json" "SkiaSharp" "Plain.Package"))

(defparameter *nac-preamble*
  (dotcl-nuget-asdf:system-nuget-preamble *nac-system*))

;;; Every declaration is asked for, in the order it was written.
(deftest nuget-asdf.preamble-names-every-package
  (let ((forms '())
        (pos 0))
    (loop (multiple-value-bind (form next)
              (read-from-string *nac-preamble* nil :eof :start pos)
            (when (eq form :eof) (return))
            (push form forms)
            (setq pos next)))
    (mapcar (lambda (f)
              ;; (eval-when (...) (cl:require "nuget") (funcall (find-symbol ...) NAME . args))
              (let ((call (car (last f))))
                (cons (third call) (nthcdr 3 call))))
            (reverse forms)))
  (("Newtonsoft.Json" :version "13.*")
   ("SkiaSharp" :version "2.88.7" :prerelease t :rid "win-arm64")
   ("Plain.Package")))

;;; The preamble has to be readable in an image where the NUGET package does not
;;; exist yet -- it is the form that creates it. Spelling the call NUGET:REQUIRE
;;; would make the reader fail before that form ever ran.
(deftest nuget-asdf.preamble-reads-without-the-nuget-package
  (let ((*package* (find-package "CL-USER")))
    (notnot (search "(find-symbol \"REQUIRE\" \"NUGET\")" *nac-preamble*)))
  t)

;;; A system that declares none gets none: the build must not prepend anything.
(deftest nuget-asdf.no-declarations-no-preamble
  (dotcl-nuget-asdf:system-nuget-preamble
   (asdf:defsystem "nac-none" :components ()))
  nil)
