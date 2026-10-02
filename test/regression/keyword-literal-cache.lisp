;;; An evaluated keyword literal is resolved once and then reused,
;;; both in memory and in a compiled file. It used to be looked up by name in
;;; the KEYWORD package on every evaluation. These check that what the site
;;; hands back is still the one interned, exported, self-evaluating keyword.

(defun %klc-a () :klc-alpha)
(defun %klc-b () ':klc-alpha)
(defun %klc-test (x) (case x (:klc-one 1) (:klc-two 2) (t 0)))

(deftest keyword-literal-cache.same-object-every-time
  (list (eq (%klc-a) (%klc-a))
        (eq (%klc-a) (%klc-b))
        (eq (%klc-a) (find-symbol "KLC-ALPHA" "KEYWORD"))
        (eq (symbol-value (%klc-a)) (%klc-a)))
  (t t t t))

(deftest keyword-literal-cache.eq-tests
  (loop for k in '(:klc-one :klc-two :klc-three) collect (%klc-test k))
  (1 2 0))

(deftest keyword-literal-cache.make-hash-table-test
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "k" h) 1)
    (list (gethash (copy-seq "k") h) (hash-table-test h)))
  (1 equal))

;; In a compiled file the keyword is held in a static field that the loaded
;; code's type initializer fills. The keyword is uninterned between compile and
;; load, so the fasl has to create it, and the created one must be a proper
;; keyword.
(deftest-compiled-only keyword-literal-cache.fasl-creates-keyword
  (let ((src "klc-src-tmp.lisp")
        (fasl "klc-src-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-string "(defun %klc-fasl-a () :klc-fresh-in-fasl)" s)
             (terpri s)
             (write-string "(defun %klc-fasl-b () ':klc-fresh-in-fasl)" s)
             (terpri s)
             (write-string "(defun %klc-fasl-c (x) (eq x :klc-fresh-in-fasl))" s)
             (terpri s))
           (compile-file src :output-file fasl)
           (let ((old (find-symbol "KLC-FRESH-IN-FASL" "KEYWORD")))
             (when old (unintern old "KEYWORD")))
           (load fasl)
           (let ((k (funcall '%klc-fasl-a)))
             (multiple-value-bind (found status)
                 (find-symbol "KLC-FRESH-IN-FASL" "KEYWORD")
               (list (eq k found)
                     status
                     (eq k (funcall '%klc-fasl-a))
                     (eq k (funcall '%klc-fasl-b))
                     (funcall '%klc-fasl-c k)
                     (funcall '%klc-fasl-c :klc-other)
                     (eq (symbol-value k) k)
                     (keywordp k)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (t :external t t t nil t t))
