;;; A temporary local that is stored once and loaded once, with only
;;; straight-line code between that leaves the stack as it found it, stays on
;;; the evaluation stack: the assembler drops the store and the load. An inlined
;;; function's parameters are the common case ((LET ((A X) (B Y)) ...) read in
;;; order). Each pair cost an IL local and two local references; ironclad's
;;; SM3-HASH had 14,562 references against the JIT's limit of 8,000. These
;;; tests pin the values in the shapes that must keep a temporary as well as the
;;; ones that lose it, and that a compiled file's method loses its locals.

(declaim (inline %ste-add %ste-sub))
(defun %ste-add (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))
(defun %ste-sub (a b)
  (declare (type (unsigned-byte 32) a b))
  ;; B is read before A: the parameters are not loaded in the order stored.
  (ldb (byte 32 0) (- (* 3 b) a)))

(defun %ste-chain (x y z)
  (declare (type (unsigned-byte 32) x y z) (optimize speed (safety 0)))
  (let ((p (%ste-add x y)))
    (declare (type (unsigned-byte 32) p))
    (%ste-add (%ste-sub p z) (%ste-add (logxor p z) (%ste-sub x y)))))

(defvar *ste-log* '())
(defun %ste-note (v) (push v *ste-log*) v)

(defun %ste-order (x)
  ;; Arguments with side effects, evaluated left to right, around a call.
  (list (%ste-note (+ x 1)) (%ste-note (+ x 2)) (length (list (%ste-note (+ x 3))))))

(defun %ste-expected-chain (x y z)
  (flet ((add (a b) (ldb (byte 32 0) (+ a b)))
         (sub (a b) (ldb (byte 32 0) (- (* 3 b) a))))
    (let ((p (add x y)))
      (add (sub p z) (add (logxor p z) (sub x y))))))

(deftest ste-chain-values
  (list (= (%ste-chain 1 2 3) (%ste-expected-chain 1 2 3))
        (= (%ste-chain #xFFFFFFFF #xFFFFFFFE 7) (%ste-expected-chain #xFFFFFFFF #xFFFFFFFE 7))
        (= (%ste-chain 0 #x80000000 #x80000000) (%ste-expected-chain 0 #x80000000 #x80000000)))
  (t t t))

(deftest ste-argument-order
  (let ((*ste-log* '()))
    (list (%ste-order 10) (reverse *ste-log*)))
  ((11 12 1) (11 12 13)))

(defun %ste-net-list (arr)
  (loop for i below (dotnet:invoke arr "Length") collect (dotnet:invoke arr "GetValue" i)))

(deftest-compiled-only ste-fasl-method-loses-temps
  (let ((src "ste-src-tmp.lisp")
        (fasl "ste-src-tmp.fasl"))
    (unwind-protect
        (progn
          (with-open-file (s src :direction :output :if-exists :supersede)
            (format s "(in-package ~s)~%" (package-name *package*))
            ;; 511 additions in a balanced tree over the four parameters: each
            ;; expansion binds A and B to copies of its operands, read in
            ;; order. The pass only runs on a body with thousands of local
            ;; references, the size where the JIT's limit comes near.
            (format s "(defun ste-file-sum (a0 a1 a2 a3)
                         (declare (type (unsigned-byte 32) a0 a1 a2 a3)
                                  (optimize speed (safety 0)))
                         ~s)~%"
                    (let ((leaf -1))
                      (labels ((tree (depth)
                                 (if (zerop depth)
                                     (nth (mod (incf leaf) 4) '(a0 a1 a2 a3))
                                     (list '%ste-add (tree (1- depth)) (tree (1- depth))))))
                        (tree 9)))))
          (compile-file src :output-file fasl)
          (load fasl)
          (let ((flags (dotnet:static "System.Enum" "ToObject"
                                      (dotnet:resolve-type "System.Reflection.BindingFlags") 62))
                (most 0))
            (dolist (asm (%ste-net-list (dotnet:invoke (dotnet:static "System.AppDomain" "CurrentDomain")
                                                       "GetAssemblies")))
              (unless (dotnet:invoke asm "IsDynamic")
                (when (search "ste-src-tmp" (dotnet:invoke asm "Location"))
                  (dolist (ty (%ste-net-list (dotnet:invoke asm "GetTypes")))
                    (dolist (m (%ste-net-list (dotnet:invoke ty "GetMethods" flags)))
                      (let ((body (dotnet:invoke m "GetMethodBody")))
                        (when body
                          (setf most (max most (dotnet:invoke (dotnet:invoke body "LocalVariables")
                                                              "Count"))))))))))
            ;; The body declares 1,027 locals; the 1,022 copies of the inlined
            ;; parameters are gone, and what is left (under 100, too few to be
            ;; worth sharing slots) is the parameters and the other temporaries.
            ;; 128 of each parameter: 1280.
            (list (funcall 'ste-file-sum 1 2 3 4)
                  (< most 100))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (1280 t))
