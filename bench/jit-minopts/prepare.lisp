;;; bench/jit-minopts/prepare.lisp -- JIT every method of a freshly compiled system
;;;
;;; Run by run.sh, one system per process, with:
;;;   JIT_MINOPTS_SYSTEM   the ASDF system to QUICKLOAD
;;;   JIT_MINOPTS_OUT      where to write the method table (TSV)
;;;   XDG_CACHE_HOME       an empty directory, so the system and its dependencies
;;;                        are compiled from source by the compiler under test
;;;   DOTNET_TieredCompilation=0
;;;                        so that PrepareMethod compiles each method with full
;;;                        optimization, which is where RyuJIT decides to fall
;;;                        back to MinOpts
;;;
;;; After the load, every method of every fasl that was written below
;;; XDG_CACHE_HOME is passed to RuntimeHelpers.PrepareMethod, so the JIT sees all
;;; of them, not only the ones the load happened to call. The JIT's own summary
;;; (DOTNET_JitStdOutFile + DOTNET_JitDisasmSummary) then says which ones it
;;; compiled as MinOpts; run.sh joins the two.
;;;
;;; Output columns: fasl path, Type:Method, IL bytes, IL locals, arguments
;;; (including `this`).

(require "quicklisp")

(defun getenv* (name)
  (dotnet:static "System.Environment" "GetEnvironmentVariable" name))

(defun net-list (arr)
  (loop for i below (dotnet:invoke arr "Length")
        collect (dotnet:invoke arr "GetValue" i)))

(defvar *system* (getenv* "JIT_MINOPTS_SYSTEM"))
(defvar *out* (getenv* "JIT_MINOPTS_OUT"))
(defvar *cache* (getenv* "XDG_CACHE_HOME"))

(unless (and *system* *out* *cache*)
  (format *error-output* "~&prepare.lisp: set JIT_MINOPTS_SYSTEM, JIT_MINOPTS_OUT and XDG_CACHE_HOME~%")
  (uiop:quit 2))

(handler-case (ql:quickload *system* :silent t)
  (error (e)
    (format *error-output* "~&prepare.lisp: QUICKLOAD ~a failed: ~a~%" *system* e)
    (uiop:quit 1)))

(let ((flags (dotnet:static "System.Enum" "ToObject"
                            (dotnet:resolve-type "System.Reflection.BindingFlags")
                            ;; DeclaredOnly | Instance | Static | Public | NonPublic
                            62))
      (fasls 0)
      (methods 0))
  (with-open-file (out *out* :direction :output :if-exists :supersede)
    (dolist (asm (net-list (dotnet:invoke (dotnet:static "System.AppDomain" "CurrentDomain")
                                          "GetAssemblies")))
      (unless (dotnet:invoke asm "IsDynamic")
        (let ((loc (dotnet:invoke asm "Location")))
          (when (and (plusp (length loc))
                     (eql 0 (search *cache* loc)))
            (incf fasls)
            (dolist (ty (net-list (dotnet:invoke asm "GetTypes")))
              (dolist (m (net-list (dotnet:invoke ty "GetMethods" flags)))
                (let ((body (dotnet:invoke m "GetMethodBody")))
                  (unless (or (null body)
                              (dotnet:invoke m "IsAbstract")
                              (dotnet:invoke m "ContainsGenericParameters"))
                    (dotnet:static "System.Runtime.CompilerServices.RuntimeHelpers"
                                   "PrepareMethod" (dotnet:invoke m "MethodHandle"))
                    (incf methods)
                    (format out "~a	~a:~a	~d	~d	~d~%"
                            (subseq loc (length *cache*))
                            (dotnet:invoke ty "FullName") (dotnet:invoke m "Name")
                            (dotnet:invoke (dotnet:invoke body "GetILAsByteArray") "Length")
                            (dotnet:invoke (dotnet:invoke body "LocalVariables") "Count")
                            (+ (dotnet:invoke (dotnet:invoke m "GetParameters") "Length")
                               (if (dotnet:invoke m "IsStatic") 0 1))))))))))))
  (format t "~&prepare.lisp: ~a: ~d fasls, ~d methods prepared~%" *system* fasls methods)
  (when (zerop fasls)
    (format *error-output* "~&prepare.lisp: no fasl below ~a; nothing was compiled~%" *cache*)
    (uiop:quit 1)))

(uiop:quit 0)
