;;; Package units: the same library loaded more than once in one image under
;;; the same package names, the shape of two swank versions listening side by
;;; side. A unit owns the names PUSWANK*. Inside a unit those names are its own
;;; packages; everything else is the global namespace. From outside, a unit's
;;; package is UNIT!NAME, and from inside, a global package the unit hides is
;;; !NAME.
;;;
;;; Loaded here: version 1 in unit OLD (from source), version 2 in unit NEW
;;; (compile-file, then the fasl, where the image can compile), and version 1
;;; again in the global namespace.

(defvar *pu-dir* "test/regression/package-units/")

(defmacro with-pu-unit ((unit) &body body)
  (let ((old (gensym "OLD")))
    `(let ((,old (dotcl::%swap-package-unit ,unit)))
       (unwind-protect (progn ,@body)
         (dotcl::%swap-package-unit ,old)))))

(defun pu-make-unit (name)
  (let ((u (dotcl::%make-package-unit name "PUSWANK*")))
    (dotcl::%add-package-unit-special u '*modules*)
    u))

(defvar *pu-old* (pu-make-unit "OLD"))
(defvar *pu-new* (pu-make-unit "NEW"))
(defvar *pu-new-module-before-load* :unset)
(defvar *pu-new-fasl* nil)

(with-pu-unit (*pu-old*)
  (load (concatenate 'string *pu-dir* "puswank-1.lisp")))
(with-pu-unit (*pu-new*)
  (setf *pu-new-module-before-load*
        (and (member "PUSWANK-TEST-MODULE" *modules* :test #'string=) t))
  (let ((src (concatenate 'string *pu-dir* "puswank-2.lisp")))
    (if (emitting-mode-p)
        (let ((fasl (regression-temp-file "puswank-2.fasl")))
          (compile-file src :output-file fasl)
          (setf *pu-new-fasl* fasl)
          (load fasl))
        (load src))))
(load (concatenate 'string *pu-dir* "puswank-1.lisp"))

(defun pu-names (packages)
  (sort (remove-if-not (lambda (n) (search "PUSWANK" n))
                       (mapcar #'package-name packages))
        #'string<))

;;; What one connection would see: every name resolved at run time, as a
;;; client's request would be.
(defun pu-probe ()
  (let ((*package* (find-package "COMMON-LISP-USER")))
    (flet ((r (s) (read-from-string s)))
      (list
       (package-name (find-package "PUSWANK"))
       (symbol-value (r "puswank:*version*"))
       (eq (symbol-package (r "puswank::connection-info")) (find-package "PUSWANK"))
       (funcall (r "puswank:connection-info"))
       (eq (symbol-package (r "pusw::connection-info")) (find-package "PUSWANK"))
       (and (find-symbol "CASIFY-CHAR" "PUSWANK") t)
       (and (find-symbol "FIND-CYCLE" "PUSWANK") t)
       (funcall (r "puswank:conn-version") (make-instance (r "puswank:connection")))
       (prin1-to-string (r "puswank:connection-info"))
       (prin1-to-string (find-package "PUSWANK"))
       (let ((*package* *package*))
         (eval (r "(in-package \"PUSWANK\")"))
         (package-name *package*))
       (pu-names (list-all-packages))
       (and (member "PUSWANK-TEST-MODULE" *modules* :test #'string=) t)))))

;;; Every package, seen from here: its name finds it again, and a symbol of
;;; every version prints as text that reads back as that symbol.
(defun pu-round-trip ()
  (let ((*package* (find-package "COMMON-LISP-USER")))
    (list
     (loop for p in (dotcl::list-every-package)
           for n = (package-name p)
           unless (eq p (find-package n)) collect n)
     (sort (loop for p in (dotcl::list-every-package)
                 for s = (find-symbol "CONNECTION-INFO" p)
                 when (and s (eq (symbol-package s) p))
                   collect (let ((text (prin1-to-string s)))
                             (list text (eq s (read-from-string text)))))
           #'string< :key #'car))))

(defun pu-in-thread (fn)
  (dotcl:thread-join (dotcl:make-thread fn)))

(defun pu-everywhere (unit fn)
  "FN in UNIT directly, on a thread started there, and on a thread that thread
starts through the library's own backend."
  (with-pu-unit (unit)
    (list (funcall fn)
          (pu-in-thread fn)
          (pu-in-thread
           (lambda ()
             (funcall (read-from-string "puswank/backend:spawn-and-wait") fn))))))

(defun pu-same-3 (results)
  (if (and (equal (first results) (second results))
           (equal (first results) (third results)))
      (first results)
      (list :differs results)))

(deftest package-units-new-sees-itself
  (pu-same-3 (pu-everywhere *pu-new* #'pu-probe))
  ("PUSWANK" "2" t (:version "2") t nil t "2"
   "PUSWANK:CONNECTION-INFO" "#<PACKAGE \"PUSWANK\">" "PUSWANK"
   ("PUSWANK" "PUSWANK/BACKEND") t))

(deftest package-units-old-sees-itself
  (pu-same-3 (pu-everywhere *pu-old* #'pu-probe))
  ("PUSWANK" "1" t (:version "1") t t nil "1"
   "PUSWANK:CONNECTION-INFO" "#<PACKAGE \"PUSWANK\">" "PUSWANK"
   ("PUSWANK" "PUSWANK-API" "PUSWANK/BACKEND") t))

(deftest package-units-global-sees-global
  (pu-probe)
  ("PUSWANK" "1" t (:version "1") t t nil "1"
   "PUSWANK:CONNECTION-INFO" "#<PACKAGE \"PUSWANK\">" "PUSWANK"
   ("PUSWANK" "PUSWANK-API" "PUSWANK/BACKEND") t))

(deftest package-units-new-module-not-provided-by-old
  *pu-new-module-before-load*
  nil)

(deftest package-units-round-trip-in-new
  (pu-same-3 (pu-everywhere *pu-new* #'pu-round-trip))
  (nil (("!PUSWANK:CONNECTION-INFO" t)
        ("OLD!PUSWANK:CONNECTION-INFO" t)
        ("PUSWANK:CONNECTION-INFO" t))))

(deftest package-units-round-trip-in-old
  (pu-same-3 (pu-everywhere *pu-old* #'pu-round-trip))
  (nil (("!PUSWANK:CONNECTION-INFO" t)
        ("NEW!PUSWANK:CONNECTION-INFO" t)
        ("PUSWANK:CONNECTION-INFO" t))))

(deftest package-units-round-trip-global
  (pu-round-trip)
  (nil (("NEW!PUSWANK:CONNECTION-INFO" t)
        ("OLD!PUSWANK:CONNECTION-INFO" t)
        ("PUSWANK:CONNECTION-INFO" t))))

;;; A global package under an owned name that the unit does not have itself is
;;; not reachable by that name from inside, so it is not listed there either.
(deftest package-units-hidden-global
  (let ((api (find-package "PUSWANK-API")))
    (with-pu-unit (*pu-new*)
      (list (find-package "PUSWANK-API")
            (package-name api)
            (eq api (find-package (package-name api)))
            (symbol-value (read-from-string "!puswank:*version*")))))
  (nil "!PUSWANK-API" t "1"))

(deftest package-units-every-package-lists-all
  (list (pu-names (dotcl::list-every-package))
        (with-pu-unit (*pu-old*) (pu-names (dotcl::list-every-package))))
  (("NEW!PUSWANK" "NEW!PUSWANK/BACKEND" "OLD!PUSWANK" "OLD!PUSWANK-API"
    "OLD!PUSWANK/BACKEND" "PUSWANK" "PUSWANK-API" "PUSWANK/BACKEND")
   ("!PUSWANK" "!PUSWANK-API" "!PUSWANK/BACKEND" "NEW!PUSWANK" "NEW!PUSWANK/BACKEND"
    "PUSWANK" "PUSWANK-API" "PUSWANK/BACKEND")))

(deftest package-units-nicknames
  (list (package-nicknames (find-package "NEW!PUSWANK"))
        (with-pu-unit (*pu-new*) (package-nicknames (find-package "PUSWANK")))
        (package-name (find-package "NEW!PUSW")))
  (("NEW!PUSW") ("PUSW") "NEW!PUSWANK"))

;;; Code loaded in a unit names its packages so that it finds the same ones
;;; when it runs outside the unit, on any thread.
(deftest package-units-code-runs-outside-its-unit
  (let ((info (with-pu-unit (*pu-new*)
                (fdefinition (read-from-string "puswank:connection-info")))))
    (list (funcall info)
          (pu-in-thread info)
          (symbol-value (read-from-string "new!puswank:*version*"))))
  ((:version "2") (:version "2") "2"))

;;; A name the unit does not own makes a global package, even inside the unit.
(deftest package-units-unowned-name-is-global
  (let ((p (with-pu-unit (*pu-old*) (make-package "PU-MADE-IN-UNIT" :use nil))))
    (prog1 (list (package-name p) (eq p (find-package "PU-MADE-IN-UNIT"))
                 (with-pu-unit (*pu-old*) (package-name p)))
      (delete-package p)))
  ("PU-MADE-IN-UNIT" t "PU-MADE-IN-UNIT"))

(deftest package-units-rename-and-delete
  (let ((u (dotcl::%make-package-unit "TMP" "PUTMP*")))
    (with-pu-unit (u)
      (let ((p (make-package "PUTMP-A" :use nil)))
        (rename-package p "PUTMP-B" '("PUTMP-C"))
        (list (find-package "PUTMP-A")
              (eq p (find-package "PUTMP-B"))
              (eq p (find-package "PUTMP-C"))
              (with-pu-unit (nil)
                (list (package-name p) (eq p (find-package "TMP!PUTMP-B"))
                      (find-package "TMP!PUTMP-A") (find-package "PUTMP-B")))
              (progn (delete-package p)
                     (list (find-package "PUTMP-B") (find-package "PUTMP-C")
                           (with-pu-unit (nil) (find-package "TMP!PUTMP-B"))))))))
  (nil t t ("TMP!PUTMP-B" t nil nil) (nil nil nil)))
;;; A fasl names packages as the unit it was compiled in saw them. Loaded where
;;; one of those names means another package, LOAD signals instead of binding
;;; the code to the wrong one.

(defun pu-compile (name &optional unit)
  (let ((fasl (regression-temp-file
               (concatenate 'string name (if unit "-unit" "") ".fasl"))))
    (with-pu-unit (unit)
      (compile-file (concatenate 'string *pu-dir* name ".lisp") :output-file fasl))
    fasl))

(defun pu-load-mismatch-p (fasl &optional unit)
  (with-pu-unit (unit)
    (handler-case (progn (load fasl) nil)
      (dotcl:fasl-version-mismatch () :mismatch))))

(deftest-emitting-only package-units-fasl-from-unit-elsewhere
  (list (pu-load-mismatch-p *pu-new-fasl*)
        (pu-load-mismatch-p *pu-new-fasl* *pu-old*)
        (pu-load-mismatch-p *pu-new-fasl* *pu-new*))
  (:mismatch :mismatch nil))

(deftest-emitting-only package-units-global-fasl-in-unit
  (let ((fasl (pu-compile "pu-lazy-global")))
    (list (pu-load-mismatch-p fasl *pu-old*)
          (pu-load-mismatch-p fasl)))
  (:mismatch nil))

(deftest-emitting-only package-units-unowned-fasl-anywhere
  (let ((fasl (pu-compile "pushared")))
    (list (pu-load-mismatch-p fasl *pu-old*)
          (pu-load-mismatch-p fasl *pu-new*)
          (pu-load-mismatch-p fasl)
          (eq (find-package "PUSHARED")
              (with-pu-unit (*pu-old*) (find-package "PUSHARED")))))
  (nil nil nil t))

(deftest-emitting-only package-units-fasl-load-as-is
  (handler-bind ((dotcl:fasl-version-mismatch #'dotcl:load-as-is))
    (with-pu-unit (*pu-old*)
      (and (load (pu-compile "pu-lazy-global")) t)))
  t)

;;; Compiled code resolves a symbol the first time it runs, in whatever unit
;;; that is; it must still find the package it was compiled against.
(deftest-emitting-only package-units-fasl-resolves-where-compiled
  (progn
    (with-pu-unit (*pu-new*) (load (pu-compile "pu-lazy" *pu-new*)))
    (load (pu-compile "pu-lazy-global"))
    (let ((own (with-pu-unit (*pu-new*) (fdefinition (read-from-string "puswank::lazy-syms"))))
          (global (fdefinition (read-from-string "puswank::lazy-global-syms")))
          (new (find-package "NEW!PUSWANK"))
          (old (find-package "OLD!PUSWANK"))
          (g (find-package "PUSWANK")))
      (destructuring-bind (a b c d)
          (with-pu-unit (*pu-old*)
            (append (funcall own) (funcall global)))
        (list (eq (symbol-package a) new) (eq (symbol-package b) g)
              (eq (symbol-package c) g) (eq (symbol-package d) g)
              (eq (symbol-package c) old)))))
  (t t t t nil))

;;; Lisp code .NET calls back into (a delegate, an event handler, a method of a
;;; class defined in Lisp) runs in the unit that was current where it was made,
;;; whichever thread .NET calls it on: the unit's packages and its specials.

(with-pu-unit (*pu-new*) (provide "PUSWANK-NEW-ONLY"))

(defun pu-mini ()
  (list (symbol-value (read-from-string "puswank:*version*"))
        (and (member "PUSWANK-NEW-ONLY" *modules* :test #'string=) t)))

(deftest package-units-delegate-called-outside
  (let* ((r nil)
         (del (with-pu-unit (*pu-new*)
                (dotnet:make-delegate "System.Action"
                                      (lambda ()
                                        (provide "PUSWANK-FROM-CALLBACK")
                                        (setf r (pu-mini)))))))
    (dotnet:invoke del "Invoke")
    (list r (pu-mini)
          (and (member "PUSWANK-FROM-CALLBACK" *modules* :test #'string=) t)
          (with-pu-unit (*pu-new*)
            (and (member "PUSWANK-FROM-CALLBACK" *modules* :test #'string=) t))))
  (("2" t) ("1" nil) nil t))

(defun pu-run-dotnet-thread (thread)
  (dotnet:invoke thread "Start")
  (dotnet:invoke thread "Join"))

;;; A thread .NET starts outside the unit, and one started inside it: the
;;; second inherits the current unit with the execution context, but not the
;;; bindings of the unit's specials.
(deftest package-units-dotnet-threads
  (let* ((r1 nil) (r2 nil)
         (del (with-pu-unit (*pu-new*)
                (dotnet:make-delegate "System.Threading.ThreadStart"
                                      (lambda () (setf r1 (pu-mini)))))))
    (pu-run-dotnet-thread (dotnet:new "System.Threading.Thread" del))
    (with-pu-unit (*pu-new*)
      (pu-run-dotnet-thread
       (dotnet:new "System.Threading.Thread" (lambda () (setf r2 (pu-mini))))))
    (list r1 r2))
  (("2" t) ("2" t)))

;;; Defining a .NET class needs the emitting runtime.
(when (emitting-mode-p)
  (dotnet:%define-class "DotclTest.PuRaiser" nil nil nil nil nil nil nil
    '(("Clicked" "System.EventHandler"))))

(deftest-emitting-only package-units-event-handler
  (let* ((r nil)
         (obj (dotnet:new "DotclTest.PuRaiser"))
         (handler (lambda (sender args)
                    (declare (ignore sender args))
                    (setf r (pu-mini)))))
    (with-pu-unit (*pu-new*) (dotnet:add-event obj "Clicked" handler))
    (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
    (prog1 r
      (with-pu-unit (*pu-new*) (dotnet:remove-event obj "Clicked" handler))
      (setf r :removed)
      (dotnet:invoke obj "OnClicked" (dotnet:new "System.EventArgs"))
      (assert (eq r :removed))))
  ("2" t))

(defvar *pu-class-result* nil)

(when (emitting-mode-p)
  (with-pu-unit (*pu-new*)
    (dotnet:%define-class "DotclTest.PuUnitClass" nil nil nil
      (list (list "Probe" "System.Int32" nil
                  (lambda (self)
                    (declare (ignore self))
                    (setf *pu-class-result* (pu-mini))
                    0))))))

(deftest-emitting-only package-units-define-class-method
  (progn
    (dotnet:invoke (dotnet:new "DotclTest.PuUnitClass") "Probe")
    *pu-class-result*)
  ("2" t))

;;; A dependency no unit owns is shared: required from two units and from
;;; outside, it is loaded once; its specials, classes and generic functions are
;;; the same objects everywhere, and each unit's own classes extend them.

(defun pu-require-common ()
  (require "PU-COMMON" (concatenate 'string *pu-dir* "pu-common.lisp")))

(defun pu-has-module-p (name)
  (and (member name *modules* :test #'string=) t))

(deftest package-units-shared-dependency-loaded-once
  (progn
    (with-pu-unit (*pu-old*) (pu-require-common))
    (list (with-pu-unit (*pu-new*) (list (pu-require-common) (pu-has-module-p "PU-COMMON")))
          (list (pu-require-common) (pu-has-module-p "PU-COMMON"))
          (symbol-value (find-symbol "*LOADS*" "PU-COMMON"))
          (eq (find-package "PU-COMMON")
              (with-pu-unit (*pu-new*) (find-package "PU-COMMON")))))
  ((nil t) (nil t) 1 t))

(defun pu-define-extension (unit tag)
  (with-pu-unit (unit)
    (eval (read-from-string
           (format nil "(progn
              (defclass puswank::ext-thing (pu-common:thing) ())
              (defmethod pu-common:describe-it ((x puswank::ext-thing))
                (list ~S (call-next-method))))" tag)))
    (make-instance (read-from-string "puswank::ext-thing") :tag tag)))

(deftest package-units-shared-dependency-state
  (let ((bump (find-symbol "BUMP" "PU-COMMON"))
        (old-thing (pu-define-extension *pu-old* :old))
        (new-thing (pu-define-extension *pu-new* :new))
        (describe-it (find-symbol "DESCRIBE-IT" "PU-COMMON")))
    (with-pu-unit (*pu-old*) (funcall bump))
    (with-pu-unit (*pu-new*) (funcall bump))
    (funcall bump)
    (list (symbol-value (find-symbol "*COUNTER*" "PU-COMMON"))
          (funcall describe-it old-thing)
          (with-pu-unit (*pu-old*) (funcall describe-it new-thing))
          (with-pu-unit (*pu-new*)
            (typep old-thing (read-from-string "pu-common:thing")))
          (eq (class-of old-thing) (class-of new-thing))))
  (3 (:old (:thing :old)) (:new (:thing :new)) t nil))
