;;; contrib/asdf-extra-deps: a system whose files use a macro and a special
;;; variable from a file they do not depend on. The audit (two builds in child
;;; processes, in ASDF's order and in the reverse order its dependencies allow)
;;; names both, and the supplement it returns makes a build in the reverse
;;; order give what a build in ASDF's order gives.

(require "asdf")
(require "asdf-extra-deps")
(load "contrib/asdf-extra-deps/audit.lisp")

(defparameter *xdeps-files*
  '(("package" "(defpackage :~a (:use :cl) (:export #:run))")
    ("a" "(in-package :~a)
          (defmacro twice (x) `(* 2 ,x))
          (defvar *depth* 1)
          (defun depth () *depth*)")
    ("b" "(in-package :~a)
          (defun use-twice () (twice 21))")
    ("c" "(in-package :~a)
          (defun bind-depth () (let ((*depth* 5)) (depth)))
          (defun run () (list (ignore-errors (use-twice)) (bind-depth)))")))

(defun %xdeps-write (name)
  "Write system NAME (its package is named the same) and return its directory."
  (let ((dir (format nil "~a/dotcl-xdeps-~a/" (regression-temp-dir) name)))
    (ensure-directories-exist dir)
    (with-open-file (s (concatenate 'string dir name ".asd")
                       :direction :output :if-exists :supersede)
      (format s "(defsystem ~s :components ((:file \"package\")
  (:file \"a\" :depends-on (\"package\")) (:file \"b\" :depends-on (\"package\"))
  (:file \"c\" :depends-on (\"package\"))))" name))
    (loop for (file text) in *xdeps-files*
          do (with-open-file (s (concatenate 'string dir file ".lisp")
                                :direction :output :if-exists :supersede)
               (format s text name)))
    dir))

(defun %xdeps-audit (name)
  (let* ((dir (%xdeps-write name))
         (report (make-string-output-stream))
         (forms (asdf-extra-deps-audit:audit
                 name
                 :lisp (list (dotnet:static "System.Environment" "ProcessPath")
                             "--core" (regression-child-core))
                 :work (concatenate 'string dir "audit/")
                 :compare nil
                 :setup (format nil "(push ~s asdf:*central-registry*)" dir)
                 :stream report)))
    (values forms (get-output-stream-string report))))

(defun %xdeps-build-reversed (name supplement)
  "Build system NAME in the reverse order in this image, with SUPPLEMENT in
   effect, and return what its RUN returns."
  (let ((dir (%xdeps-write name))
        (reg (find-symbol "*CENTRAL-REGISTRY*" "ASDF")))
    (push dir (symbol-value reg))
    (asdf-extra-deps:clear-supplements)
    (when supplement (asdf-extra-deps:add-supplement supplement))
    (unwind-protect
         (handler-bind ((warning #'muffle-warning))
           (asdf:load-system name :force t
                                  :plan-class (find-symbol "AUDIT-PLAN" "ASDF-EXTRA-DEPS-AUDIT")
                                  :plan-options '(:order :reverse))
           (funcall (find-symbol "RUN" (string-upcase name))))
      (asdf-extra-deps:clear-supplements)
      (setf (symbol-value reg) (remove dir (symbol-value reg) :test #'equal))
      (asdf:clear-system name))))

(defvar *xdeps-forms* nil)

(deftest-emitting-only asdf-extra-deps.audit-finds
  (multiple-value-bind (forms report) (%xdeps-audit "xdeps-audit")
    (setf *xdeps-forms* forms)
    (list (mapcar (lambda (f) (getf (cddr f) :depends-on)) forms)
          (= 64 (length (first (getf (cddr (first forms)) :asd-sha256))))
          (and (search "UNDECLARED xdeps-audit/b -> xdeps-audit/a" report)
               (search "macro XDEPS-AUDIT:TWICE" report)
               (search "UNDECLARED xdeps-audit/c -> xdeps-audit/a" report)
               (search "special XDEPS-AUDIT:*DEPTH*" report)
               (search "WEAK       xdeps-audit/c -> xdeps-audit/b" report)
               t)))
  (((("b" "a") ("c" "a"))) t t))

(deftest-emitting-only asdf-extra-deps.reverse-without
  (%xdeps-build-reversed "xdeps-plain" nil)
  (nil 1))

(deftest-emitting-only asdf-extra-deps.reverse-with
  (let ((form (copy-list (first *xdeps-forms*))))
    ;; The same files under another name: only the .asd's hash differs.
    (setf (second form) "xdeps-fixed")
    (setf (getf (cddr form) :asd-sha256) nil)
    (%xdeps-build-reversed "xdeps-fixed" form))
  (42 5))

(deftest asdf-extra-deps.guard
  (let ((dir (%xdeps-write "xdeps-guard")))
    (push dir asdf:*central-registry*)
    (unwind-protect
         (let* ((sys (asdf:find-system "xdeps-guard"))
                (b (asdf:find-component sys "b"))
                (sha (asdf-extra-deps:file-sha256 (asdf:system-source-file sys))))
           (flet ((extra (hash)
                    (asdf-extra-deps:clear-supplements)
                    (asdf-extra-deps:add-supplement
                     `(:system "xdeps-guard" :asd-sha256 (,hash)
                       :depends-on (("b" "a" "nowhere"))))
                    (prog1 (mapcar #'asdf:component-name
                                   (asdf-extra-deps:extra-dependencies b))
                      (asdf-extra-deps:clear-supplements))))
             (list (extra sha)
                   (extra (make-string 64 :initial-element #\0))
                   (asdf-extra-deps::sha256-hex
                    (map '(vector (unsigned-byte 8)) #'char-code "abc")))))
      (setf asdf:*central-registry* (remove dir asdf:*central-registry* :test #'equal))
      (asdf:clear-system "xdeps-guard")))
  (("a") nil "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
