;;; FIND-SYMBOL, INTERN, the symbol property list and the symbol accessors stay
;;; at their floors. The allocation assertions are about emitted code, so they
;;; are compiled-only: the interpreter does not use the direct entries whose
;;; cost is measured here.

(defparameter *sppr-n* 1000)
(defparameter *sppr-sink* nil)

(defparameter *sppr-short* "aa")
(defparameter *sppr-long* (make-string 40 :initial-element #\a))
(defparameter *sppr-nonascii*
  (let ((s (make-string 8)))
    (setf (char s 0) (code-char 233)
          (char s 1) (code-char 945)
          (char s 2) (code-char 946)
          (char s 3) (code-char 947)
          (char s 4) (code-char 948)
          (char s 5) (code-char 949)
          (char s 6) (code-char 950)
          (char s 7) (code-char 951))
    s))

(defparameter *sppr-pkg* (make-package "SPPR-PKG" :use nil))
(defparameter *sppr-use* (make-package "SPPR-USE" :use nil))
(defparameter *sppr-big* (make-package "SPPR-BIG" :use nil))

(defparameter *sppr-ext* (intern *sppr-short* *sppr-pkg*))
(defparameter *sppr-long-sym* (intern *sppr-long* *sppr-pkg*))
(defparameter *sppr-nonascii-sym* (intern *sppr-nonascii* *sppr-pkg*))
(dolist (n (list "s0" "s1" "s2" "s3" "s4" "s5" "s6"))
  (intern n *sppr-pkg*))
(export *sppr-ext* *sppr-pkg*)
(use-package *sppr-pkg* *sppr-use*)
(loop for i below 5000
      do (intern (format nil "q~D" i) *sppr-big*))

(defparameter *sppr-keys1* (list (intern "SPPR-K1-0" :keyword)))
(defparameter *sppr-keys4*
  (loop for i below 4 collect (intern (format nil "SPPR-K4-~D" i) :keyword)))
(defparameter *sppr-keys16*
  (loop for i below 16 collect (intern (format nil "SPPR-K16-~D" i) :keyword)))
(defparameter *sppr-plist1* (list (first *sppr-keys1*) 0))
(defparameter *sppr-plist4*
  (loop for k in *sppr-keys4* for i from 0 append (list k i)))
(defparameter *sppr-plist16*
  (loop for k in *sppr-keys16* for i from 0 append (list k i)))
(defparameter *sppr-miss* :sppr-no-such-indicator)
(defparameter *sppr-default* :sppr-default)

(defparameter *sppr-get1* (make-symbol "SPPR-GET-1"))
(defparameter *sppr-get4* (make-symbol "SPPR-GET-4"))
(defparameter *sppr-get16* (make-symbol "SPPR-GET-16"))
(defparameter *sppr-wsym4* (make-symbol "SPPR-SETF-4"))
(defparameter *sppr-wsym16* (make-symbol "SPPR-SETF-16"))
(defparameter *sppr-isym* (make-symbol "SPPR-INCF"))
(defparameter *sppr-psym* (make-symbol "SPPR-PUSH"))
(defparameter *sppr-rsym* (make-symbol "SPPR-REMPROP"))
(defparameter *sppr-plsym* (make-symbol "SPPR-PLIST"))
(defparameter *sppr-plsym2* (make-symbol "SPPR-PLIST-SET"))
(defparameter *sppr-push-list* (list :old))

(setf (symbol-plist *sppr-get1*) *sppr-plist1*
      (symbol-plist *sppr-get4*) *sppr-plist4*
      (symbol-plist *sppr-get16*) *sppr-plist16*
      (symbol-plist *sppr-wsym4*) *sppr-plist4*
      (symbol-plist *sppr-wsym16*) *sppr-plist16*
      (symbol-plist *sppr-isym*) (list (first *sppr-keys4*) 0)
      (symbol-plist *sppr-psym*) (list (first *sppr-keys4*) *sppr-push-list*)
      (symbol-plist *sppr-plsym*) *sppr-plist4*
      (symbol-plist *sppr-plsym2*) *sppr-plist4*)

(defparameter *sppr-vsym* (intern "SPPR-GLOBAL-VALUE" *sppr-pkg*))
(setf (symbol-value *sppr-vsym*) 7)
(defparameter *sppr-fsym* (intern "SPPR-FUNCTION" *sppr-pkg*))
(setf (symbol-function *sppr-fsym*) #'identity)
(defparameter *sppr-esym* (intern "SPPR-EQ-SYMBOL" *sppr-pkg*))

(defun %sppr-baseline ()
  (min-allocated-bytes (lambda ()
                         (dotimes (i *sppr-n*) (setq *sppr-sink* nil)))
                       5))

(defun %sppr-failures (rows)
  (let ((base (%sppr-baseline)) (bad nil))
    (dolist (row rows (nreverse bad))
      (funcall (third row))
      (let ((got (min-allocated-bytes (third row) 5)))
        (when (> got (+ base (* *sppr-n* (+ (cadr row) 16))))
          (push (list (car row) got base (cadr row)) bad))))))

(defmacro %sppr-row (label floor form)
  `(list ,label ,floor
         (lambda () (dotimes (i *sppr-n*) (setq *sppr-sink* ,form)))))

(deftest-compiled-only symbol-package-plist-allocation.symbols
  (%sppr-failures
   (list
    (%sppr-row "FIND-SYMBOL-INTERNAL" 0 (find-symbol *sppr-long* *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-EXTERNAL" 0 (find-symbol *sppr-short* *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-INHERITED" 0 (find-symbol *sppr-short* *sppr-use*))
    (%sppr-row "FIND-SYMBOL-ABSENT" 0 (find-symbol "sppr-no-such-symbol" *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-INTERNAL-STATUS" 0
               (multiple-value-bind (sym status) (find-symbol *sppr-long* *sppr-pkg*)
                 (declare (ignore status)) sym))
    (%sppr-row "FIND-SYMBOL-EXTERNAL-STATUS" 0
               (multiple-value-bind (sym status) (find-symbol *sppr-short* *sppr-pkg*)
                 (declare (ignore status)) sym))
    (%sppr-row "FIND-SYMBOL-INHERITED-STATUS" 0
               (multiple-value-bind (sym status) (find-symbol *sppr-short* *sppr-use*)
                 (declare (ignore status)) sym))
    (%sppr-row "FIND-SYMBOL-ABSENT-STATUS" 0
               (multiple-value-bind (sym status) (find-symbol "sppr-no-such-symbol" *sppr-pkg*)
                 (declare (ignore status)) sym))
    (%sppr-row "FIND-SYMBOL-OMITTED" 0
               (let ((*package* *sppr-pkg*)) (find-symbol *sppr-long*)))
    (%sppr-row "FIND-SYMBOL-OMITTED-STATUS" 0
               (let ((*package* *sppr-pkg*))
                 (multiple-value-bind (sym status) (find-symbol *sppr-long*)
                   (declare (ignore status)) sym)))
    (%sppr-row "FIND-SYMBOL-STRING-DESIGNATOR" 0 (find-symbol *sppr-long* "SPPR-PKG"))
    (%sppr-row "FIND-SYMBOL-KEYWORD-PACKAGE" 0 (find-symbol *sppr-long* :sppr-pkg))
    (%sppr-row "FIND-SYMBOL-PKG10" 0 (find-symbol "s0" *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-PKG5000" 0 (find-symbol "q4999" *sppr-big*))
    (%sppr-row "FIND-SYMBOL-COPY-SEQ-SHORT" 64
               (find-symbol (copy-seq *sppr-short*) *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-COPY-SEQ-LONG" 136
               (find-symbol (copy-seq *sppr-long*) *sppr-pkg*))
    (%sppr-row "FIND-SYMBOL-COPY-SEQ-NONASCII" 72
               (find-symbol (copy-seq *sppr-nonascii*) *sppr-pkg*))
    (%sppr-row "INTERN-INTERNAL" 0 (intern *sppr-long* *sppr-pkg*))
    (%sppr-row "INTERN-EXTERNAL" 0 (intern *sppr-short* *sppr-pkg*))
    (%sppr-row "INTERN-INHERITED" 0 (intern *sppr-short* *sppr-use*))
    (%sppr-row "INTERN-INTERNAL-STATUS" 0
               (multiple-value-bind (sym status) (intern *sppr-long* *sppr-pkg*)
                 (declare (ignore status)) sym))
    (%sppr-row "INTERN-EXTERNAL-STATUS" 0
               (multiple-value-bind (sym status) (intern *sppr-short* *sppr-pkg*)
                 (declare (ignore status)) sym))
    (%sppr-row "INTERN-INHERITED-STATUS" 0
               (multiple-value-bind (sym status) (intern *sppr-short* *sppr-use*)
                 (declare (ignore status)) sym))
    (%sppr-row "INTERN-OMITTED" 0
               (let ((*package* *sppr-pkg*)) (intern *sppr-long*)))
    (%sppr-row "INTERN-OMITTED-STATUS" 0
               (let ((*package* *sppr-pkg*))
                 (multiple-value-bind (sym status) (intern *sppr-long*)
                   (declare (ignore status)) sym)))
    (%sppr-row "INTERN-STRING-DESIGNATOR" 0 (intern *sppr-long* "SPPR-PKG"))
    (%sppr-row "INTERN-KEYWORD-PACKAGE" 0 (intern *sppr-long* :sppr-pkg))
    (%sppr-row "INTERN-PKG10" 0 (intern "s0" *sppr-pkg*))
    (%sppr-row "INTERN-PKG5000" 0 (intern "q4999" *sppr-big*))
    (%sppr-row "INTERN-COPY-SEQ-SHORT" 64
               (intern (copy-seq *sppr-short*) *sppr-pkg*))
    (%sppr-row "INTERN-COPY-SEQ-LONG" 136
               (intern (copy-seq *sppr-long*) *sppr-pkg*))
    (%sppr-row "INTERN-COPY-SEQ-NONASCII" 72
               (intern (copy-seq *sppr-nonascii*) *sppr-pkg*))))
  nil)

(defmacro %sppr-get-row (label sym key)
  `(list
    (%sppr-row ,(concatenate 'string label "-NO-DEFAULT") 0 (get ,sym ,key))
    (%sppr-row ,(concatenate 'string label "-DEFAULT") 0
               (get ,sym ,key *sppr-default*))))

(deftest-compiled-only symbol-package-plist-allocation.get
  (%sppr-failures
   (append
    (%sppr-get-row "GET-1-FIRST" *sppr-get1* (first *sppr-keys1*))
    (%sppr-get-row "GET-1-MISS" *sppr-get1* *sppr-miss*)
    (%sppr-get-row "GET-4-FIRST" *sppr-get4* (first *sppr-keys4*))
    (%sppr-get-row "GET-4-MIDDLE" *sppr-get4* (nth 1 *sppr-keys4*))
    (%sppr-get-row "GET-4-LAST" *sppr-get4* (nth 3 *sppr-keys4*))
    (%sppr-get-row "GET-4-MISS" *sppr-get4* *sppr-miss*)
    (%sppr-get-row "GET-16-FIRST" *sppr-get16* (first *sppr-keys16*))
    (%sppr-get-row "GET-16-MIDDLE" *sppr-get16* (nth 8 *sppr-keys16*))
    (%sppr-get-row "GET-16-LAST" *sppr-get16* (nth 15 *sppr-keys16*))
    (%sppr-get-row "GET-16-MISS" *sppr-get16* *sppr-miss*)))
  nil)

(deftest-compiled-only symbol-package-plist-allocation.updates
  (%sppr-failures
   (list
    (%sppr-row "SETF-GET-REPLACE-4" 0
               (setf (get *sppr-wsym4* (nth 1 *sppr-keys4*)) 0))
    (%sppr-row "SETF-GET-REPLACE-16" 0
               (setf (get *sppr-wsym16* (nth 8 *sppr-keys16*)) 0))
    (%sppr-row "SETF-GET-ADD-4" 64
               (progn
                 (setf (symbol-plist *sppr-wsym4*) *sppr-plist4*)
                 (setf (get *sppr-wsym4* :sppr-new-key) 0)))
    (%sppr-row "SETF-GET-ADD-16" 64
               (progn
                 (setf (symbol-plist *sppr-wsym16*) *sppr-plist16*)
                 (setf (get *sppr-wsym16* :sppr-new-key) 0)))
    (%sppr-row "GET-INCF" 0
               (progn
                 (setf (get *sppr-isym* (first *sppr-keys4*)) 0)
                 (incf (get *sppr-isym* (first *sppr-keys4*)))))
    (%sppr-row "GET-PUSH" 32
               (progn
                 (setf (get *sppr-psym* (first *sppr-keys4*)) *sppr-push-list*)
                 (push :new (get *sppr-psym* (first *sppr-keys4*)))))
    (%sppr-row "REMPROP-1-FIRST" 64
               (progn
                 (setf (symbol-plist *sppr-rsym*) (list (first *sppr-keys1*) 0))
                 (remprop *sppr-rsym* (first *sppr-keys1*))))
    (%sppr-row "REMPROP-1-MISS" 64
               (progn
                 (setf (symbol-plist *sppr-rsym*) (list (first *sppr-keys1*) 0))
                 (remprop *sppr-rsym* *sppr-miss*)))
    (%sppr-row "REMPROP-4-FIRST" 256
               (progn
                 (setf (symbol-plist *sppr-rsym*)
                       (list (nth 0 *sppr-keys4*) 0 (nth 1 *sppr-keys4*) 1
                             (nth 2 *sppr-keys4*) 2 (nth 3 *sppr-keys4*) 3))
                 (remprop *sppr-rsym* (first *sppr-keys4*))))
    (%sppr-row "REMPROP-4-MIDDLE" 256
               (progn
                 (setf (symbol-plist *sppr-rsym*)
                       (list (nth 0 *sppr-keys4*) 0 (nth 1 *sppr-keys4*) 1
                             (nth 2 *sppr-keys4*) 2 (nth 3 *sppr-keys4*) 3))
                 (remprop *sppr-rsym* (nth 1 *sppr-keys4*))))
    (%sppr-row "REMPROP-4-LAST" 256
               (progn
                 (setf (symbol-plist *sppr-rsym*)
                       (list (nth 0 *sppr-keys4*) 0 (nth 1 *sppr-keys4*) 1
                             (nth 2 *sppr-keys4*) 2 (nth 3 *sppr-keys4*) 3))
                 (remprop *sppr-rsym* (nth 3 *sppr-keys4*))))
    (%sppr-row "REMPROP-4-MISS" 256
               (progn
                 (setf (symbol-plist *sppr-rsym*)
                       (list (nth 0 *sppr-keys4*) 0 (nth 1 *sppr-keys4*) 1
                             (nth 2 *sppr-keys4*) 2 (nth 3 *sppr-keys4*) 3))
                 (remprop *sppr-rsym* *sppr-miss*)))
    (%sppr-row "REMPROP-16-FIRST" 1024
               (progn
                 (setf (symbol-plist *sppr-rsym*) (copy-list *sppr-plist16*))
                 (remprop *sppr-rsym* (first *sppr-keys16*))))
    (%sppr-row "REMPROP-16-MIDDLE" 1024
               (progn
                 (setf (symbol-plist *sppr-rsym*) (copy-list *sppr-plist16*))
                 (remprop *sppr-rsym* (nth 8 *sppr-keys16*))))
    (%sppr-row "REMPROP-16-LAST" 1024
               (progn
                 (setf (symbol-plist *sppr-rsym*) (copy-list *sppr-plist16*))
                 (remprop *sppr-rsym* (nth 15 *sppr-keys16*))))
    (%sppr-row "REMPROP-16-MISS" 1024
               (progn
                 (setf (symbol-plist *sppr-rsym*) (copy-list *sppr-plist16*))
                 (remprop *sppr-rsym* *sppr-miss*)))
    (%sppr-row "SYMBOL-PLIST-READ" 0 (symbol-plist *sppr-plsym*))
    (%sppr-row "SYMBOL-PLIST-SET" 0
               (setf (symbol-plist *sppr-plsym2*) *sppr-plist16*))))
  nil)

(deftest-compiled-only symbol-package-plist-allocation.accessors
  (%sppr-failures
   (list
    (%sppr-row "SYMBOL-NAME" 0 (symbol-name *sppr-long-sym*))
    (%sppr-row "SYMBOL-NAME-NONASCII" 0 (symbol-name *sppr-nonascii-sym*))
    (%sppr-row "SYMBOL-PACKAGE" 0 (symbol-package *sppr-long-sym*))
    (%sppr-row "SYMBOL-VALUE" 0 (symbol-value *sppr-vsym*))
    (%sppr-row "SETF-SYMBOL-VALUE" 0 (setf (symbol-value *sppr-vsym*) 7))
    (%sppr-row "BOUNDP" 0 (boundp *sppr-vsym*))
    (%sppr-row "FBOUNDP" 0 (fboundp *sppr-fsym*))
    (%sppr-row "SYMBOL-FUNCTION" 0 (symbol-function *sppr-fsym*))
    (%sppr-row "KEYWORDP" 0 (keywordp :sppr-kw))
    (%sppr-row "MAKE-SYMBOL" 152 (make-symbol *sppr-long*))
    (%sppr-row "STRING-SYMBOL" 0 (string *sppr-long-sym*))
    (%sppr-row "EQ-SYMBOL" 0 (eq *sppr-esym* *sppr-esym*))
    (%sppr-row "TYPEP-SYMBOL" 0 (typep *sppr-long-sym* 'symbol))))
  nil)

;;; --- behaviour of the changed paths ---

(deftest symbol-copy-seq-is-an-independent-copy
  (let* ((orig (let ((s (make-string 4)))
                 (setf (char s 0) (code-char 233)
                       (char s 1) #\b (char s 2) #\c (char s 3) #\d)
                 s))
         (copy (copy-seq orig)))
    (list (eq orig copy)
          (string= orig copy)
          (progn (setf (char copy 0) #\z) (char orig 0))
          (char copy 0)))
  (nil t #.(code-char 233) #\z))

(deftest symbol-copy-seq-finds-nonascii-name
  (multiple-value-bind (sym status)
      (find-symbol (copy-seq *sppr-nonascii*) *sppr-pkg*)
    (list (eq sym *sppr-nonascii-sym*) status))
  (t :internal))

(deftest symbol-find-status-paths
  (list (multiple-value-bind (sym status) (find-symbol "s0" *sppr-pkg*)
          (list (symbol-name sym) status))
        (multiple-value-bind (sym status) (find-symbol *sppr-short* *sppr-pkg*)
          (list (symbol-name sym) status))
        (multiple-value-bind (sym status) (find-symbol *sppr-short* *sppr-use*)
          (list (symbol-name sym) status))
        (multiple-value-list (find-symbol "SPPR-NO-SUCH-NAME" *sppr-pkg*)))
  (("s0" :internal) ("aa" :external) ("aa" :inherited) (nil nil)))

(deftest symbol-intern-status-paths
  (list (multiple-value-bind (sym status) (intern "s1" *sppr-pkg*)
          (list (symbol-name sym) status))
        (multiple-value-bind (sym status) (intern *sppr-short* *sppr-pkg*)
          (list (symbol-name sym) status))
        (multiple-value-bind (sym status) (intern *sppr-short* *sppr-use*)
          (list (symbol-name sym) status))
        (multiple-value-bind (sym status) (intern "SPPR-NEW-NAME" *sppr-pkg*)
          (list (symbol-name sym) status)))
  (("s1" :internal) ("aa" :external) ("aa" :inherited) ("SPPR-NEW-NAME" nil)))

(deftest symbol-find-omitted-status-path
  (let ((*package* *sppr-pkg*))
    (multiple-value-bind (sym status) (find-symbol *sppr-long*)
      (list (eq sym *sppr-long-sym*) status)))
  (t :internal))

(deftest symbol-intern-omitted-status-path
  (let ((*package* *sppr-pkg*))
    (multiple-value-bind (sym status) (intern *sppr-long*)
      (list (eq sym *sppr-long-sym*) status)))
  (t :internal))

(deftest symbol-package-lookup-is-case-sensitive
  (list (multiple-value-list (find-symbol "S0" *sppr-pkg*))
        (multiple-value-list (find-symbol "s0" *sppr-pkg*)))
  ((nil nil) (#.(intern "s0" "SPPR-PKG") :internal)))

(deftest symbol-plist-setf-returns-value
  (let* ((sym (make-symbol "SPPR-PLIST-BEHAVIOUR"))
         (plist (list :a 1 :b 2)))
    (list (eq (setf (symbol-plist sym) plist) plist)
          (eq (symbol-plist sym) plist)))
  (t t))
