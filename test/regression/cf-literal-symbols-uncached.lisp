;;; In a fasl, a literal is built once, by the first execution of its site, and
;;; kept in a static field. A symbol inside that literal is looked up by name
;;; while it is built, and that lookup is not given a cache field of its own:
;;; the literal's field already makes it run once. These check that the literal
;;; still comes back as one object with its symbols in the package the file
;;; made, and that the fasl no longer carries a per-symbol field for them.

(defun %cflsu-array-list (a)
  (loop for i below (dotnet:invoke a "get_Length")
        collect (dotnet:invoke a "GetValue" i)))

(defun %cflsu-count-fields (fasl prefix)
  (let ((asm (dotnet:static "System.Reflection.Assembly" "LoadFile"
                            (namestring (truename fasl))))
        (n 0))
    (dolist (ty (%cflsu-array-list (dotnet:invoke asm "GetTypes")) n)
      (dolist (f (%cflsu-array-list (dotnet:invoke ty "GetFields")))
        (let ((nm (dotnet:invoke f "get_Name")))
          (when (and (>= (length nm) (length prefix))
                     (string= prefix nm :end2 (length prefix)))
            (incf n)))))))

(defun %cflsu-compile (name text)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only cf-literal-symbols-uncached.values
  (let ((fasl (%cflsu-compile "cflsu-a"
                              "(defpackage :cflsu-p (:use :cl))
(in-package :cflsu-p)
(defun lits () '(alpha (beta . gamma) #(delta epsilon) cl:car :kw \"str\" 12))
(defvar *top* '(zeta eta))
(defun quasi (x) `(alpha ,x beta))
")))
    (load fasl)
    (let* ((lits (find-symbol "LITS" "CFLSU-P"))
           (quasi (find-symbol "QUASI" "CFLSU-P"))
           (a (funcall lits))
           (p (find-package "CFLSU-P")))
      (list (eq a (funcall lits))
            (eq (symbol-package (first a)) p)
            (eq (symbol-package (cdr (second a))) p)
            (eq (symbol-package (aref (third a) 1)) p)
            (eq (fourth a) 'car)
            (eq (fifth a) :kw)
            (mapcar (lambda (s) (eq (symbol-package s) p))
                    (symbol-value (find-symbol "*TOP*" "CFLSU-P")))
            (let ((q (funcall quasi 1)))
              (list (eq (symbol-package (first q)) p) (second q)
                    (eq (third q) (third (funcall quasi 2))))))))
  (t t t t t t (t t) (t 1 t)))

(deftest-compiled-only cf-literal-symbols-uncached.no-symbol-fields
  ;; Six symbols, all inside the one literal. Only the defun's own name is
  ;; referenced outside it (the value the top level form returns).
  (let ((fasl (%cflsu-compile "cflsu-b"
                              "(defpackage :cflsu-q (:use :cl))
(in-package :cflsu-q)
(defun lits () '(alpha (beta . gamma) #(delta epsilon) cl:car))
")))
    (load fasl)
    (list (let ((*package* (find-package "CFLSU-Q")))
            (prin1-to-string (funcall (find-symbol "LITS" "CFLSU-Q"))))
          (<= (%cflsu-count-fields fasl "_symp_") 1)))
  ("(ALPHA (BETA . GAMMA) #(DELTA EPSILON) CAR)" t))
