;;; A compiled file's method with more than 256 locals must not carry padding.
;;;
;;; ldloc / stloc / ldloca for a local numbered 256 or more take a 2-byte
;;; operand. The fasl writer's IL encoder wrote a 4-byte one, so the JIT read the
;;; two extra zero bytes as two NOPs after every such access. The code still
;;; ran, but the extra bytes and instructions pushed large functions (ironclad's
;;; unrolled digests and ciphers) past the JIT's size limits, where it stops
;;; optimizing. The test compiles a function whose 300 locals are all live at
;;; once, loads the fasl, and reads back the method's IL.

(defun flo-net-list (arr)
  (loop for i below (dotnet:invoke arr "Length") collect (dotnet:invoke arr "GetValue" i)))

(defun flo-fasl-methods (marker)
  "Methods with an IL body in the loaded assemblies whose location contains MARKER."
  (let ((flags (dotnet:static "System.Enum" "ToObject"
                              (dotnet:resolve-type "System.Reflection.BindingFlags") 62))
        (out '()))
    (dolist (asm (flo-net-list (dotnet:invoke (dotnet:static "System.AppDomain" "CurrentDomain")
                                              "GetAssemblies"))
                 out)
      (unless (dotnet:invoke asm "IsDynamic")
        (when (search marker (dotnet:invoke asm "Location"))
          (dolist (ty (flo-net-list (dotnet:invoke asm "GetTypes")))
            (dolist (m (flo-net-list (dotnet:invoke ty "GetMethods" flags)))
              (when (dotnet:invoke m "GetMethodBody")
                (push m out)))))))))

(deftest-compiled-only fasl-long-local-operand.no-nop-padding
  (let* ((n 300)
         (vars (loop for i below n collect (intern (format nil "FLO-V~d" i))))
         (form `(defun flo-many-locals (x)
                  (declare (type (unsigned-byte 8) x))
                  (let* ,(loop for v in vars for i from 0
                               collect `(,v (logand (+ x ,i) 255)))
                    (declare (type (unsigned-byte 8) ,@vars))
                    (+ ,@vars))))
         (src "flo-src-tmp.lisp")
         (fasl "flo-src-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(in-package ~s)~%" (package-name *package*))
            (let ((*print-circle* nil)) (format s "~s~%" form)))
          (compile-file src :output-file fasl)
          (load fasl)
          (let* ((methods (flo-fasl-methods "flo-src-tmp"))
                 (big (remove-if-not
                       (lambda (m)
                         (> (dotnet:invoke (dotnet:invoke (dotnet:invoke m "GetMethodBody")
                                                          "LocalVariables")
                                           "Count")
                            256))
                       methods))
                 (nops (loop for m in big
                             sum (count-if (lambda (x) (and (consp x) (eq (car x) :nop)))
                                           (dotnet:static "DotCL.Emitter.IlDisasm"
                                                          "DisassembleMethod" m)))))
            (list (funcall 'flo-many-locals 7)
                  (and big t)
                  nops)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  ;; the sum over i < 300 of (7 + i) mod 256, a 300-local method, no NOP
  (33894 t 0))
