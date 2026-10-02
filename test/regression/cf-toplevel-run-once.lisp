;;; A fasl runs each top level form once, from a helper method of its own. When
;;; that helper has no backward branch, none of its code runs twice, so its
;;; literals and symbols are built in place instead of through cache fields that
;;; would be filled once and never read. A top level form with a loop keeps the
;;; cache fields: a literal evaluated on every iteration must stay one object.

(defun %cftro-array-list (a)
  (loop for i below (dotnet:invoke a "get_Length")
        collect (dotnet:invoke a "GetValue" i)))

(defun %cftro-count-fields (fasl prefix)
  (let ((asm (dotnet:static "System.Reflection.Assembly" "LoadFile"
                            (namestring (truename fasl))))
        (n 0))
    (dolist (ty (%cftro-array-list (dotnet:invoke asm "GetTypes")) n)
      (dolist (f (%cftro-array-list (dotnet:invoke ty "GetFields")))
        (let ((nm (dotnet:invoke f "get_Name")))
          (when (and (>= (length nm) (length prefix))
                     (string= prefix nm :end2 (length prefix)))
            (incf n)))))))

(defun %cftro-compile (name text)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only cf-toplevel-run-once.loop-keeps-literal-identity
  (let ((fasl (%cftro-compile "cftro-a"
                              "(defpackage :cftro-p (:use :cl))
(in-package :cftro-p)
(defparameter *loop* (loop repeat 3 collect '(alpha beta)))
(defparameter *tag* (let ((n 0) (acc '()))
                      (tagbody
                       again
                         (push '(gamma) acc)
                         (when (< (incf n) 3) (go again)))
                      acc))
(defparameter *once* (list '(delta epsilon) 'zeta))
")))
    (load fasl)
    (let ((p (find-package "CFTRO-P")))
      (flet ((v (name) (symbol-value (find-symbol name p))))
        (let ((l (v "*LOOP*")) (tg (v "*TAG*")) (once (v "*ONCE*")))
          (list (length l)
                (and (eq (first l) (second l)) (eq (second l) (third l)))
                (eq (symbol-package (first (first l))) p)
                (length tg)
                (and (eq (first tg) (second tg)) (eq (second tg) (third tg)))
                (let ((*package* p)) (prin1-to-string once))
                (eq (symbol-package (second once)) p))))))
  (3 t t 3 t "((DELTA EPSILON) ZETA)" t))

(deftest-compiled-only cf-toplevel-run-once.no-cache-fields
  ;; Only loop-free top level forms: nothing in the fasl needs a literal or a
  ;; symbol cache field.
  (let ((fasl (%cftro-compile "cftro-b"
                              "(defpackage :cftro-q (:use :cl))
(in-package :cftro-q)
(defparameter *a* '(alpha (beta . gamma) #(delta)))
(setf (get 'alpha 'prop) '(epsilon zeta))
")))
    (load fasl)
    (let ((p (find-package "CFTRO-Q")))
      (list (let ((*package* p))
              (list (prin1-to-string (symbol-value (find-symbol "*A*" p)))
                    (prin1-to-string (get (find-symbol "ALPHA" p) (find-symbol "PROP" p)))))
            (%cftro-count-fields fasl "_lit_")
            (%cftro-count-fields fasl "_symp_"))))
  (("(ALPHA (BETA . GAMMA) #(DELTA))" "(EPSILON ZETA)") 0 0))
