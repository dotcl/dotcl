;;; dotcl-gpu.lisp: run a Lisp function over arrays on an ILGPU accelerator.
;;;
;;;   (require "dotcl-gpu")
;;;   (dotcl-gpu:gpu-map '(lambda (x) (* 2d0 x))
;;;                      (make-array 4 :element-type 'double-float
;;;                                    :initial-contents '(1d0 2d0 3d0 4d0)))
;;;   ;; => #(2.0d0 4.0d0 6.0d0 8.0d0)
;;;
;;; GPU-MAP compiles the lambda with dotcl, reads the IL of its typed entry (the
;;; static method dotcl emits for a function whose arguments and result are
;;; double-floats), re-emits it without the function-object parameter as a device
;;; function, and gives ILGPU a kernel that applies it to every element. Only
;;; straight-line double arithmetic and System.Math calls can be re-emitted: a
;;; function with a branch, a loop, a call to anything else, or a boxed value
;;; signals UNSUPPORTED-KERNEL.
;;;
;;; ILGPU is restored from NuGet the first time a kernel is built (.NET SDK and
;;; network), not when this module is required. It reads IL at run time, so it
;;; needs a build with code generation. The CPU accelerator is the one used.

(require "dotnet-nuget")
(require "dotcl-thread")

(defpackage :dotcl-gpu
  (:use :cl)
  (:export #:gpu-map #:ensure-ilgpu #:unsupported-kernel #:unsupported-kernel-instruction
           #:*ilgpu-version* #:clear-kernel-cache))

(in-package :dotcl-gpu)

(defvar *ilgpu-version* "1.5.1")

(define-condition unsupported-kernel (error)
  ((instruction :initarg :instruction :reader unsupported-kernel-instruction)
   (reason :initarg :reason :reader %reason))
  (:report (lambda (c s)
             (format s "GPU-MAP: cannot run this function on a device: ~a~@[ (~a)~]"
                     (%reason c) (unsupported-kernel-instruction c)))))

(defun unsupported (reason &optional instruction)
  (error 'unsupported-kernel :reason reason :instruction instruction))

(defun ensure-ilgpu ()
  (nuget:require "ILGPU" :version *ilgpu-version*))

;;; ---------------------------------------------------------------- reading IL

(defvar *opcodes* nil)

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

(defun method-il (method)
  "The IL of METHOD (a System.Reflection.MethodInfo) as a list of (NAME . OPERAND):
a number for a constant or a variable index, the System.Reflection.MethodBase for a
call, NIL for an instruction without an operand."
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
                                          (dotnet:invoke module "ResolveMethod" v)
                                          v))))))
                     (push (cons name operand) out))))))
    (nreverse out)))

(defun local-types (method)
  (let ((locals (dotnet:invoke (dotnet:invoke method "GetMethodBody") "LocalVariables")))
    (loop for i below (dotnet:invoke locals "Count")
          collect (dotnet:invoke (dotnet:invoke locals "Item" i) "LocalType"))))

;;; ------------------------------------------------------------ emitting it back

(defun op (name) (dotnet:static "System.Reflection.Emit.OpCodes" name))

(defun emit-without-self (gen method)
  "Emit the IL of METHOD, a typed entry, into the ILGenerator GEN with its first
parameter (the function object) removed. Signals UNSUPPORTED-KERNEL for anything
but straight-line double arithmetic and System.Math calls."
  (let ((locals (loop for ty in (local-types method)
                      do (unless (string= (dotnet:invoke ty "FullName") "System.Double")
                           (unsupported "a local that is not a double-float"
                                        (dotnet:invoke ty "FullName")))
                      collect (dotnet:invoke gen "DeclareLocal" ty))))
    (flet ((emit (name &rest args) (apply #'dotnet:invoke gen "Emit" (op name) args))
           (local (name operand)
             (let ((k (or operand (digit-char-p (char name (1- (length name)))))))
               (or (nth k locals) (unsupported "a local out of range" name)))))
      (dolist (ins (method-il method))
        (let ((name (car ins)) (operand (cdr ins)))
          (cond ((and (= (length name) 7) (string= (subseq name 0 6) "ldarg.")
                      (digit-char-p (char name 6)) (<= 1 (digit-char-p (char name 6)) 3))
                 (emit (format nil "Ldarg_~d" (1- (digit-char-p (char name 6))))))
                ((member name '("stloc.0" "stloc.1" "stloc.2" "stloc.3" "stloc.s")
                         :test #'string=)
                 (emit "Stloc" (local name operand)))
                ((member name '("ldloc.0" "ldloc.1" "ldloc.2" "ldloc.3" "ldloc.s")
                         :test #'string=)
                 (emit "Ldloc" (local name operand)))
                ((member name '("mul" "add" "sub" "div" "neg" "ret") :test #'string=)
                 (emit (string-capitalize name)))
                ((string= name "ldc.r8") (emit "Ldc_R8" operand))
                ((and (string= name "call")
                      (string= (dotnet:invoke (dotnet:invoke operand "DeclaringType") "FullName")
                               "System.Math"))
                 (emit "Call" operand))
                (t (unsupported "an instruction a device function cannot use" name))))))))

(defun double-type () (dotnet:resolve-type "System.Double"))

(defun view-type ()
  (dotnet:make-generic-type "ILGPU.Runtime.ArrayView1D`2"
                            (list "System.Double" (dotnet:resolve-type "ILGPU.Stride1D+Dense"))))

(defun build-kernel-type (typed-entry n-inputs)
  "A dynamic type DK with the device function Helper and the kernel
Map(i, out, in1 ... inN): out[i] = Helper(in1[i] ... inN[i])."
  (let* ((assembly (dotnet:static "System.Reflection.Emit.AssemblyBuilder" "DefineDynamicAssembly"
                                  (dotnet:new "System.Reflection.AssemblyName" "DotclGpuKernels")
                                  (dotnet:enum-or "System.Reflection.Emit.AssemblyBuilderAccess" "Run")))
         (module (dotnet:invoke assembly "DefineDynamicModule" "DotclGpuKernels"))
         (type (dotnet:invoke module "DefineType" "DK"
                              (dotnet:enum-or "System.Reflection.TypeAttributes" "Public" "Abstract" "Sealed")))
         (double (double-type))
         (index (dotnet:resolve-type "ILGPU.Index1D"))
         (view (view-type))
         (attrs (dotnet:enum-or "System.Reflection.MethodAttributes" "Public" "Static"))
         (helper (dotnet:invoke type "DefineMethod" "Helper" attrs double
                                (apply #'dotnet:new-array "System.Type"
                                       (loop repeat n-inputs collect double))))
         (kernel (dotnet:invoke type "DefineMethod" "Map" attrs (dotnet:resolve-type "System.Void")
                                (apply #'dotnet:new-array "System.Type"
                                       index (loop repeat (1+ n-inputs) collect view))))
         (get-item (dotnet:invoke view "GetMethod" "get_Item" (dotnet:new-array "System.Type" index))))
    (emit-without-self (dotnet:invoke helper "GetILGenerator") typed-entry)
    (let ((g (dotnet:invoke kernel "GetILGenerator")))
      (flet ((emit (name &rest args) (apply #'dotnet:invoke g "Emit" (op name) args)))
        ;; arguments: 0 = i, 1 = out, 2.. = the inputs
        (emit "Ldarga" 1) (emit "Ldarg_0") (emit "Call" get-item)
        (dotimes (k n-inputs)
          (emit "Ldarga" (+ 2 k)) (emit "Ldarg_0") (emit "Call" get-item) (emit "Ldind_R8"))
        (emit "Call" helper)
        (emit "Stind_R8")
        (emit "Ret")))
    (dotnet:invoke type "CreateType")))

;;; --------------------------------------------------------- compiling the lambda

(defvar *counter* 0)
(defvar *lock* (dotcl-thread:make-lock "dotcl-gpu"))

(defun temp-file (name type)
  (merge-pathnames (make-pathname :name name :type type)
                   (pathname (dotnet:static "System.IO.Path" "GetTempPath"))))

(defun compile-typed-entry (lambda-form n)
  "Compile LAMBDA-FORM, a function of N double-floats giving a double-float, with
COMPILE-FILE and return the System.Reflection.MethodInfo of its typed entry."
  (destructuring-bind (lambda-symbol lambda-list &body body) lambda-form
    (declare (ignore lambda-symbol))
    (let* ((id (dotcl-thread:with-lock-held (*lock*) (incf *counter*)))
           (name (format nil "GPU-KERNEL-~d-~d" (dotnet:static "System.Environment" "ProcessId") id))
           (sym (intern name :cl-user))
           (src (temp-file (string-downcase name) "lisp"))
           (fasl (temp-file (string-downcase name) "fasl")))
      (unless (and (listp lambda-list) (= (length lambda-list) n)
                   (every #'symbolp lambda-list))
        (unsupported "the lambda list must name exactly one plain variable per array"))
      (unwind-protect
           (progn
             (with-open-file (s src :direction :output :if-exists :supersede)
               (let ((*package* (find-package :cl-user)) (*print-circle* nil)
                     (*print-readably* nil) (*print-pretty* nil))
                 (prin1 `(declaim (ftype (function ,(loop repeat n collect 'double-float) double-float)
                                         ,sym))
                        s)
                 (terpri s)
                 (prin1 `(defun ,sym ,lambda-list
                           (declare (optimize (debug 0) (safety 0))
                                    (double-float ,@lambda-list))
                           ,@body)
                        s)
                 (terpri s)))
             (let ((*error-output* (make-broadcast-stream))
                   (*standard-output* (make-broadcast-stream)))
               (compile-file src :output-file fasl))
             (let* ((assembly (dotnet:static "System.Reflection.Assembly" "LoadFrom" (namestring fasl)))
                    (type (dotnet:invoke assembly "GetType" "CompiledModule"))
                    (prefix (substitute #\_ #\- (string-upcase name)))
                    (methods (dotnet:invoke type "GetMethods"
                                            (dotnet:enum-or "System.Reflection.BindingFlags"
                                                            "Public" "NonPublic" "Static"
                                                            "DeclaredOnly"))))
               (dotimes (i (dotnet:invoke methods "Length")
                           (unsupported "dotcl emitted no typed entry for this function"))
                 (let ((m (dotnet:invoke methods "GetValue" i)))
                   (when (eql 0 (search (concatenate 'string prefix "_typed") (dotnet:invoke m "Name")))
                     (return m))))))
        ;; The assembly stays loaded; only the source is not needed any more.
        (ignore-errors (delete-file src))))))

;;; ------------------------------------------------------------------ ILGPU glue

(defvar *accelerator* nil)
(defvar *kernels* (make-hash-table :test 'equal)
  "(lambda-form . n-inputs) -> the kernel delegate.")

(defun accelerator ()
  (or *accelerator*
      (progn
        (ensure-ilgpu)
        (setf *accelerator*
              (dotnet:static "ILGPU.Runtime.CPU.CPUContextExtensions" "CreateCPUAccelerator"
                             (dotnet:static "ILGPU.Context" "CreateDefault") 0)))))

(defun clear-kernel-cache ()
  (clrhash *kernels*))

(defun generic-method (type name n-params predicate)
  (let ((methods (dotnet:invoke (dotnet:resolve-type type) "GetMethods")))
    (dotimes (i (dotnet:invoke methods "Length") (error "dotcl-gpu: no ~a.~a" type name))
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

(defun load-kernel (lambda-form n-inputs)
  (let* ((entry (compile-typed-entry lambda-form n-inputs))
         (kernel-type (build-kernel-type entry n-inputs))
         (index (dotnet:resolve-type "ILGPU.Index1D"))
         (view (view-type))
         (view-args (loop repeat (1+ n-inputs) collect view))
         (action (dotnet:make-generic-type (format nil "System.Action`~d" (+ 2 n-inputs))
                                           (cons index view-args)))
         (delegate (dotnet:static "System.Delegate" "CreateDelegate" action
                                  (dotnet:invoke kernel-type "GetMethod" "Map"))))
    (dotnet:static-generic "ILGPU.Runtime.KernelLoaders" "LoadAutoGroupedStreamKernel"
                           (cons index view-args) (accelerator) delegate)))

(defun double-vector-p (x)
  (typep x '(simple-array double-float (*))))

(defun to-net-array (vector)
  (let ((array (dotnet:static "System.Array" "CreateInstance" (double-type) (length vector))))
    (dotnet:static "System.Buffer" "BlockCopy" vector 0 array 0 (* 8 (length vector)))
    array))

(defun gpu-map (function &rest arrays)
  "Apply FUNCTION, a lambda expression of as many double-float arguments as there
are ARRAYS (one to three), to the elements of the ARRAYS on an ILGPU accelerator, and
return the results as a new (simple-array double-float (*)). The ARRAYS must be
(simple-array double-float (*)) of one length. FUNCTION is compiled once and kept,
so a later call with an EQUAL lambda expression reuses its kernel."
  (let ((n (length arrays)))
    (unless (<= 1 n 3) (error "GPU-MAP: one to three arrays, not ~d" n))
    (unless (and (consp function) (eq (car function) 'lambda))
      (error "GPU-MAP: FUNCTION must be a lambda expression, not ~s" function))
    (dolist (a arrays)
      (unless (double-vector-p a)
        (error "GPU-MAP: ~s is not a (simple-array double-float (*))" a)))
    (let ((len (length (first arrays))))
      (unless (every (lambda (a) (= (length a) len)) arrays)
        (error "GPU-MAP: the arrays differ in length"))
      (let* ((accelerator (accelerator))
             (key (cons function n))
             (kernel (or (gethash key *kernels*)
                         (setf (gethash key *kernels*) (load-kernel function n))))
             (copy (generic-method "ILGPU.Runtime.ArrayViewExtensions" "CopyFromCPU" 2
                                   (lambda (ps) (and (dense-view-p (first ps))
                                                     (dotnet:invoke (second ps) "IsArray")))))
             (get (generic-method "ILGPU.Runtime.ArrayViewExtensions" "CopyToCPU" 2
                                  (lambda (ps) (and (dense-view-p (first ps))
                                                    (dotnet:invoke (second ps) "IsArray")))))
             (out (make-array len :element-type 'double-float :initial-element 0d0)))
        (if (zerop len)
            out
            (let ((buffers (loop repeat (1+ n)
                                 collect (dotnet:invoke-generic accelerator "Allocate1D"
                                                                '("System.Double") len))))
              (unwind-protect
                   (progn
                     (loop for a in arrays
                           for b in (rest buffers)
                           do (call-generic copy '("System.Double") (dotnet:invoke b "View")
                                            (to-net-array a)))
                     (apply #'dotnet:invoke kernel "Invoke" (dotnet:new "ILGPU.Index1D" len)
                            (mapcar (lambda (b) (dotnet:invoke b "View")) buffers))
                     (dotnet:invoke accelerator "Synchronize")
                     (let ((result (dotnet:static "System.Array" "CreateInstance" (double-type) len)))
                       (call-generic get '("System.Double") (dotnet:invoke (first buffers) "View") result)
                       (dotnet:static "System.Buffer" "BlockCopy" result 0 out 0 (* 8 len))
                       out))
                (dolist (b buffers) (dotnet:invoke b "Dispose"))))))))) 

(provide "dotcl-gpu")
