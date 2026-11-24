;;; A spike: run a function that dotcl compiled on an ILGPU accelerator (its CPU
;;; backend), to see whether ILGPU accepts the IL dotcl emits.
;;;
;;; What it does:
;;;   1. A Lisp function with an FTYPE of double-floats gets a typed entry in the
;;;      fasl (a static method taking and returning raw doubles; its first
;;;      parameter is the function object, unused by straight-line code).
;;;   2. The typed entry's IL is read back through reflection and re-emitted with
;;;      System.Reflection.Emit into a dynamic assembly, without that first
;;;      parameter, as a static device function.
;;;   3. A small kernel (also emitted here) calls it for every element:
;;;      y[i] = f(a, x[i], y[i]), and ILGPU compiles and runs the kernel.
;;;
;;; ILGPU rejects the typed entry as it comes (it cannot pass the function object:
;;; "Not supported instruction Ldnull"), and rejects any entry that calls into the
;;; runtime's multiple-values bookkeeping ("Cannot load from the static field ...
;;; since it is not read only"). Straight-line double arithmetic and Math.Sqrt
;;; pass.
;;;
;;; Needs the .NET SDK and the network the first time (NuGet restores ILGPU).
;;;
;;;   (load "samples/GpuSpike/ilgpu-spike.lisp")
;;;   (gpu-spike:run-typed-entry "my.fasl" "AXPY_typed" 2d0 :n 1000)

(require "dotnet-nuget")

(defpackage :gpu-spike
  (:use :cl)
  (:export #:ensure-ilgpu #:typed-entry-il #:find-typed-entry #:run-typed-entry #:*ilgpu-version*))

(in-package :gpu-spike)

(defvar *ilgpu-version* "1.5.1")

(defun ensure-ilgpu ()
  (nuget:require "ILGPU" :version *ilgpu-version*))

;;; ---------------------------------------------------------------- reading IL

(defvar *opcodes* nil
  "Code -> (name operand-type) for every System.Reflection.Emit.OpCode.")

(defun opcode-table ()
  (or *opcodes*
      (let ((table (make-hash-table))
            (fields (dotnet:invoke (dotnet:resolve-type "System.Reflection.Emit.OpCodes")
                                   "GetFields")))
        (dotimes (i (dotnet:invoke fields "Length"))
          (let ((op (dotnet:invoke (dotnet:invoke fields "GetValue" i) "GetValue" nil)))
            (setf (gethash (logand #xffff (dotnet:invoke op "Value")) table)
                  (list (dotnet:invoke op "Name")
                        (dotnet:invoke (dotnet:invoke op "OperandType") "ToString")))))
        (setf *opcodes* table))))

(defun typed-entry-il (method)
  "The IL of METHOD (a System.Reflection.MethodInfo) as a list of (NAME . OPERAND):
OPERAND is a number for a constant or a variable index, a string \"Type::Method\" for a
call, and NIL for an instruction without one. Branches are not followed: a method
that has any is returned with its branch offsets as operands, and cannot be re-emitted
by EMIT-WITHOUT-SELF."
  (let* ((body (dotnet:invoke method "GetMethodBody"))
         (il (dotnet:invoke body "GetILAsByteArray"))
         (module (dotnet:invoke method "Module"))
         (n (dotnet:invoke il "Length"))
         (pos 0)
         (out '()))
    (flet ((byte-at (k) (dotnet:invoke il "GetValue" k))
           (int32 (k) (+ (dotnet:invoke il "GetValue" k)
                         (* 256 (dotnet:invoke il "GetValue" (+ k 1)))
                         (* 65536 (dotnet:invoke il "GetValue" (+ k 2)))
                         (* 16777216 (dotnet:invoke il "GetValue" (+ k 3))))))
      (loop while (< pos n)
            do (let ((code (byte-at pos)))
                 (incf pos)
                 (when (= code #xfe)
                   (setf code (+ #xfe00 (byte-at pos)))
                   (incf pos))
                 (destructuring-bind (name optype)
                     (or (gethash code (opcode-table)) (list (format nil "?~x" code) "InlineNone"))
                   (let ((operand
                           (cond ((string= optype "InlineNone") nil)
                                 ((member optype '("ShortInlineBrTarget" "ShortInlineI" "ShortInlineVar")
                                          :test #'string=)
                                  (prog1 (byte-at pos) (incf pos)))
                                 ((string= optype "InlineR")
                                  (prog1 (dotnet:static "System.BitConverter" "ToDouble" il pos)
                                    (incf pos 8)))
                                 ((string= optype "InlineI8")
                                  (prog1 (dotnet:static "System.BitConverter" "ToInt64" il pos)
                                    (incf pos 8)))
                                 (t (let ((v (int32 pos)))
                                      (incf pos 4)
                                      (if (string= optype "InlineMethod")
                                          (let ((m (dotnet:invoke module "ResolveMethod" v)))
                                            (format nil "~a::~a"
                                                    (dotnet:invoke (dotnet:invoke m "DeclaringType") "Name")
                                                    (dotnet:invoke m "Name")))
                                          v))))))
                     (push (cons name operand) out))))))
    (nreverse out)))

;;; ------------------------------------------------------------ emitting it back

(defun op (name) (dotnet:static "System.Reflection.Emit.OpCodes" name))

(defun emit-without-self (gen instructions n-locals)
  "Emit INSTRUCTIONS, a typed entry's IL, into the ILGenerator GEN with the entry's
first parameter (the function object) removed. N-LOCALS double locals are declared.
Handles straight-line double arithmetic, ldc.r8 and calls to System.Math."
  (dotimes (i n-locals)
    (dotnet:invoke gen "DeclareLocal" (dotnet:resolve-type "System.Double")))
  (flet ((emit (name &rest args) (apply #'dotnet:invoke gen "Emit" (op name) args)))
    (dolist (ins instructions)
      (let ((name (car ins)) (operand (cdr ins)))
        (cond ((member name '("ldarg.1" "ldarg.2" "ldarg.3") :test #'string=)
               (emit (format nil "Ldarg_~d" (1- (digit-char-p (char name 6))))))
              ((member name '("stloc.0" "stloc.1" "stloc.2" "stloc.3" "ldloc.0" "ldloc.1" "ldloc.2"
                              "ldloc.3" "mul" "add" "sub" "div" "neg" "ret")
                       :test #'string=)
               (emit (string-capitalize (substitute #\_ #\. name))))
              ((member name '("stloc.s" "ldloc.s") :test #'string=)
               (emit (if (string= name "stloc.s") "Stloc" "Ldloc") operand))
              ((string= name "ldc.r8") (emit "Ldc_R8" operand))
              ((and (string= name "call") (string= operand "Math::Sqrt"))
               (emit "Call" (dotnet:invoke (dotnet:resolve-type "System.Math") "GetMethod" "Sqrt"
                                           (dotnet:new-array "System.Type"
                                                             (dotnet:resolve-type "System.Double")))))
              (t (error "gpu-spike: cannot re-emit ~a~@[ ~a~]" name operand)))))))

(defun build-kernel-type (instructions n-locals)
  "A dynamic type DK with the device function Helper(a, x, y) and the kernel
Daxpy(i, x, y, a): y[i] = Helper(a, x[i], y[i])."
  (let* ((assembly (dotnet:static "System.Reflection.Emit.AssemblyBuilder" "DefineDynamicAssembly"
                                  (dotnet:new "System.Reflection.AssemblyName" "GpuSpikeKernels")
                                  (dotnet:enum-or "System.Reflection.Emit.AssemblyBuilderAccess" "Run")))
         (module (dotnet:invoke assembly "DefineDynamicModule" "GpuSpikeKernels"))
         (type (dotnet:invoke module "DefineType" "DK"
                              (dotnet:enum-or "System.Reflection.TypeAttributes" "Public" "Abstract" "Sealed")))
         (double (dotnet:resolve-type "System.Double"))
         (index (dotnet:resolve-type "ILGPU.Index1D"))
         (dense (dotnet:resolve-type "ILGPU.Stride1D+Dense"))
         (view (dotnet:make-generic-type "ILGPU.Runtime.ArrayView1D`2" (list "System.Double" dense)))
         (attrs (dotnet:enum-or "System.Reflection.MethodAttributes" "Public" "Static"))
         (helper (dotnet:invoke type "DefineMethod" "Helper" attrs double
                                (dotnet:new-array "System.Type" double double double)))
         (kernel (dotnet:invoke type "DefineMethod" "Daxpy" attrs (dotnet:resolve-type "System.Void")
                                (dotnet:new-array "System.Type" index view view double)))
         (get-item (dotnet:invoke view "GetMethod" "get_Item" (dotnet:new-array "System.Type" index))))
    (emit-without-self (dotnet:invoke helper "GetILGenerator") instructions n-locals)
    (let ((g (dotnet:invoke kernel "GetILGenerator")))
      (flet ((emit (name &rest args) (apply #'dotnet:invoke g "Emit" (op name) args)))
        ;; arguments: 0 = i, 1 = x, 2 = y, 3 = a.  y[i] = Helper(a, x[i], y[i])
        (emit "Ldarga" 2) (emit "Ldarg_0") (emit "Call" get-item)
        (emit "Ldarg_3")
        (emit "Ldarga" 1) (emit "Ldarg_0") (emit "Call" get-item) (emit "Ldind_R8")
        (emit "Ldarga" 2) (emit "Ldarg_0") (emit "Call" get-item) (emit "Ldind_R8")
        (emit "Call" helper)
        (emit "Stind_R8")
        (emit "Ret")))
    (dotnet:invoke type "CreateType")))

;;; ------------------------------------------------------------------ ILGPU glue

(defun generic-method (type name n-params predicate)
  "The generic method definition NAME of TYPE with N-PARAMS parameters whose
parameter types satisfy PREDICATE (called with the list of System.Type)."
  (let ((methods (dotnet:invoke (dotnet:resolve-type type) "GetMethods")))
    (dotimes (i (dotnet:invoke methods "Length") (error "gpu-spike: no ~a.~a" type name))
      (let* ((m (dotnet:invoke methods "GetValue" i))
             (ps (dotnet:invoke m "GetParameters")))
        (when (and (equal (dotnet:invoke m "Name") name)
                   (dotnet:invoke m "IsGenericMethodDefinition")
                   (= (dotnet:invoke ps "Length") n-params)
                   (funcall predicate
                            (loop for k below n-params
                                  collect (dotnet:invoke (dotnet:invoke ps "GetValue" k)
                                                         "ParameterType"))))
          (return m))))))

(defun call-generic (method type-names &rest args)
  (let ((closed (dotnet:invoke method "MakeGenericMethod"
                               (apply #'dotnet:new-array "System.Type"
                                      (mapcar #'dotnet:resolve-type type-names)))))
    (dotnet:invoke closed "Invoke" nil (apply #'dotnet:new-array "System.Object" args))))

(defun dense-view-p (type)
  (let ((s (dotnet:invoke type "ToString")))
    (and (search "ArrayView1D" s) (search "Dense" s))))

(defun run-kernel (kernel-type n a)
  "Run KERNEL-TYPE's Daxpy on ILGPU's CPU accelerator over x[i] = i, y[i] = 1.
Returns the whole result as a list of doubles."
  (let* ((context (dotnet:static "ILGPU.Context" "CreateDefault"))
         (accelerator (dotnet:static "ILGPU.Runtime.CPU.CPUContextExtensions"
                                     "CreateCPUAccelerator" context 0))
         (index (dotnet:resolve-type "ILGPU.Index1D"))
         (dense (dotnet:resolve-type "ILGPU.Stride1D+Dense"))
         (view (dotnet:make-generic-type "ILGPU.Runtime.ArrayView1D`2" (list "System.Double" dense)))
         (delegate-type (dotnet:make-generic-type "System.Action`4"
                                                  (list index view view "System.Double")))
         (delegate (dotnet:static "System.Delegate" "CreateDelegate" delegate-type
                                  (dotnet:invoke kernel-type "GetMethod" "Daxpy")))
         (kernel (dotnet:static-generic "ILGPU.Runtime.KernelLoaders" "LoadAutoGroupedStreamKernel"
                                        (list index view view "System.Double") accelerator delegate))
         (xs (apply #'dotnet:new-array "System.Double" (loop for i below n collect (float i 1d0))))
         (ys (apply #'dotnet:new-array "System.Double" (loop repeat n collect 1d0)))
         (xb (dotnet:invoke-generic accelerator "Allocate1D" '("System.Double") n))
         (yb (dotnet:invoke-generic accelerator "Allocate1D" '("System.Double") n))
         (copy (generic-method "ILGPU.Runtime.ArrayViewExtensions" "CopyFromCPU" 2
                               (lambda (ps) (and (dense-view-p (first ps))
                                                 (dotnet:invoke (second ps) "IsArray")))))
         (get (generic-method "ILGPU.Runtime.ArrayViewExtensions" "GetAsArray1D" 1
                              (lambda (ps) (dense-view-p (first ps))))))
    (call-generic copy '("System.Double") (dotnet:invoke xb "View") xs)
    (call-generic copy '("System.Double") (dotnet:invoke yb "View") ys)
    (dotnet:invoke kernel "Invoke" (dotnet:new "ILGPU.Index1D" n)
                   (dotnet:invoke xb "View") (dotnet:invoke yb "View") a)
    (dotnet:invoke accelerator "Synchronize")
    (let ((out (call-generic get '("System.Double") (dotnet:invoke yb "View"))))
      (loop for i below n collect (dotnet:invoke out "GetValue" i)))))

(defun find-typed-entry (fasl name-prefix)
  "The static method of the fasl's CompiledModule whose name starts with NAME-PREFIX."
  (let* ((assembly (dotnet:static "System.Reflection.Assembly" "LoadFrom" (namestring fasl)))
         (type (dotnet:invoke assembly "GetType" "CompiledModule"))
         (methods (dotnet:invoke type "GetMethods"
                                 (dotnet:enum-or "System.Reflection.BindingFlags"
                                                 "Public" "NonPublic" "Static" "DeclaredOnly"))))
    (dotimes (i (dotnet:invoke methods "Length") (error "gpu-spike: no typed entry ~a" name-prefix))
      (let ((m (dotnet:invoke methods "GetValue" i)))
        (when (eql 0 (search name-prefix (dotnet:invoke m "Name")))
          (return m))))))

(defun run-typed-entry (fasl name-prefix a &key (n 1000))
  "Run the typed entry named NAME-PREFIX... of FASL, a function of three double-floats
(A, x[i], y[i]), over N elements on ILGPU's CPU accelerator: x[i] = i, y[i] = 1, and
y[i] becomes (f A x[i] y[i]). Returns the list of y."
  (ensure-ilgpu)
  (let* ((instructions (typed-entry-il (find-typed-entry fasl name-prefix)))
         (n-locals 3))
    (run-kernel (build-kernel-type instructions n-locals) n a)))
