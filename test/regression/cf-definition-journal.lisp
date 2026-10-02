;;; COMPILE-FILE tells what a compile-time evaluation defined (kept after the
;;; file is compiled) from the early definitions of plain DEFUNs (removed then)
;;; by recording the function cells assigned while the form is compiled and
;;; run, instead of comparing every symbol of every package before and after
;;; each such form (about 6 ms a scan with a few libraries loaded; generic-cl
;;; has 756 such forms).

(defun %cfdj-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defun %cfdj-open-windows ()
  (let* ((type (dotnet:static "System.Type" "GetType" "DotCL.DefinitionJournal, DotCL.Runtime"))
         (field (dotnet:invoke type "GetField" "Depth"
                               (dotnet:static "System.Enum" "Parse"
                                              (dotnet:static "System.Type" "GetType"
                                                             "System.Reflection.BindingFlags")
                                              "NonPublic, Static"))))
    (dotnet:invoke field "GetValue" nil)))

(deftest-compiled-only cf-definition-journal.kept-and-stripped
  (progn
    (%cfdj-compile "cfdj-a"
                   '("(eval-when (:compile-toplevel) (defun cfdj-ct () 1) (defun (setf cfdj-ct) (v) v))"
                     "(eval-when (:compile-toplevel :load-toplevel :execute) (defun cfdj-all () 2))"
                     "(defun cfdj-plain () 3)"
                     "(defmacro cfdj-m () (list 'quote (cfdj-plain)))"
                     "(defun cfdj-uses-m () (cfdj-m))"))
    (list (fboundp (intern "CFDJ-CT"))
          (fboundp (list 'setf (intern "CFDJ-CT")))
          (fboundp (intern "CFDJ-ALL"))
          (fboundp (intern "CFDJ-PLAIN"))
          (%cfdj-open-windows)))
  (t t t nil 0))

;; A compile-time evaluation that signals leaves no window open.
(deftest-compiled-only cf-definition-journal.error-closes-window
  (progn
    (ignore-errors
     (%cfdj-compile "cfdj-b"
                    '("(eval-when (:compile-toplevel) (defun cfdj-b1 () 1) (error \"boom\"))")))
    (%cfdj-open-windows))
  0)
