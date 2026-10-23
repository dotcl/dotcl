;;; audit.lisp -- find the dependencies an ASDF system uses but does not
;;; declare, and write them as supplements for asdf-extra-deps.
;;;
;;;   (load ".../contrib/asdf-extra-deps/audit.lisp")
;;;   (asdf-extra-deps-audit:audit "serapeum"
;;;     :lisp '("sbcl" "--non-interactive" "--load")   ; or '("dotcl")
;;;     :work "/tmp/audit/")
;;;
;;; AUDIT builds the system, with everything it depends on, twice from an empty
;;; cache, each time in a fresh Lisp started with LISP (the script to load is
;;; appended to it), and in two orders:
;;;
;;;   :plan     ASDF's own order. The build also records, for every Lisp file,
;;;             the definitions it introduced (macros, inline functions, special
;;;             variables, packages, features...), and afterwards reads every
;;;             file again and notes the symbols, packages and features it
;;;             names. A file that names something a file it does not depend on
;;;             defined, before it in this order, is a candidate.
;;;   :reverse  Another order the declared dependencies allow: of the actions
;;;             ready to run, always the one ASDF would run last. What a file
;;;             saw only by coming after another one in the :PLAN order, it does
;;;             not see here. A compile that fails is done again once everything
;;;             before it in ASDF's order is done.
;;;   :parallel contrib/asdf-parallel, with JOBS compiles at once.
;;;
;;; It then compares the fasls of the two builds: byte for byte, or on dotcl,
;;; whose fasls differ in every build, with bench/parallel-compile/fasl-digest.cs
;;; (*FASL-DIGEST*). In the :PLAN and :REVERSE orders each compile starts with
;;; *GENSYM-COUNTER* at 0, so that what was compiled before does not show in
;;; the names of uninterned symbols; against :PARALLEL, dotcl's comparison
;;; ignores the digits in those names instead.
;;;
;;; The report lists, per file, the files it should depend on, with the
;;; definitions that it uses from each and whether its fasl differed or its
;;; compile failed in the other order. A candidate through something that
;;; changes how a file compiles (a macro, an inline function, a special
;;; variable, a package, a type...) is "strong" and goes into the supplement
;;; forms AUDIT returns; one through plain functions, classes or methods only
;;; matters at run time and is listed as "weak". Weak candidates also go in
;;; when the file failed to compile in the other order.
;;;
;;; The system and its dependencies must be installed (with Quicklisp, run
;;; QL:QUICKLOAD once first). SETUP is Lisp text the children evaluate first;
;;; the default loads Quicklisp.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf")
  (unless (find-package :asdf-extra-deps)
    (load (merge-pathnames "asdf-extra-deps.lisp" (or *compile-file-truename* *load-truename*)))))

(defpackage :asdf-extra-deps-audit
  (:use :cl)
  (:export #:audit
           #:*fasl-digest*
           #:*strong-kinds*))

(in-package :asdf-extra-deps-audit)

(defvar *here* (or *compile-file-truename* *load-truename*))

(defvar *fasl-digest*
  (merge-pathnames "../../bench/parallel-compile/fasl-digest.cs" *here*)
  "The fasl-digest.cs program that compares dotcl fasls.")

(defparameter *strong-kinds*
  '(:package :feature :macro :compiler-macro :symbol-macro :inline :transform
    :setf-expander :special :constant :type)
  "Kinds of definition that change how a file that uses them compiles.")

;;; ------------------------------------------------------------------
;;; What a symbol has defined, per kind. Each value is compared with EQL
;;; between two snapshots; a change is a definition by the file loaded since.

(defparameter +kinds+
  #(:macro :function :compiler-macro :symbol-macro :inline :setf-expander
    :special :constant :type :class :setf-function :method :transform))

(defun find-sym (name package)
  (let ((p (find-package package)))
    (and p (find-symbol name p))))

(defun mop-methods (f)
  (let ((fn (or (find-sym "GENERIC-FUNCTION-METHODS" "SB-MOP")
                (find-sym "GENERIC-FUNCTION-METHODS" "DOTCL-MOP")
                (find-sym "GENERIC-FUNCTION-METHODS" "CLOSER-MOP"))))
    (and fn (typep f 'generic-function) (length (funcall fn f)))))

(defun type-defined-p (s)
  #+sbcl (eq (sb-int:info :type :kind s) :defined)
  #-sbcl
  (let ((fn (or (find-sym "TYPEXPAND-1" "DOTCL")
                (find-sym "TYPEXPAND-1" "TRIVIAL-TYPES"))))
    (and fn (handler-case (nth-value 1 (funcall fn s)) (error () t)))))

(defun inline-p (s)
  #+sbcl (eq (sb-int:info :function :inlinep s) :inline)
  #+dotcl (let ((table (symbol-value (find-sym "*INLINE-DEFS*" "DOTCL-INTERNAL"))))
            (gethash s table))
  #-(or sbcl dotcl) (declare (ignore s))
  #-(or sbcl dotcl) nil)

(defun transform-p (s)
  #+sbcl (and (sb-int:info :function :source-transform s) t)
  #+dotcl (let ((table (symbol-value (find-sym "*STRUCT-ACCESSOR-TYPES*" "DOTCL-INTERNAL"))))
            (gethash s table))
  #-(or sbcl dotcl) (declare (ignore s))
  #-(or sbcl dotcl) nil)

(defun setf-expander (s)
  #+sbcl (sb-int:info :setf :expander s)
  #+dotcl (let ((table (symbol-value (find-sym "*SETF-EXPANDERS*" "DOTCL-INTERNAL")))
                (key (funcall (find-sym "%SETF-KEY" "DOTCL.CIL-COMPILER") s)))
            (gethash key table))
  #-(or sbcl dotcl) (declare (ignore s))
  #-(or sbcl dotcl) nil)

(defun special-p (s)
  #+sbcl (member (sb-int:info :variable :kind s) '(:special :global))
  #+dotcl (funcall (find-sym "GLOBAL-SPECIAL-P" "DOTCL.CIL-COMPILER") s)
  #-(or sbcl dotcl) (boundp s))

(defun symbol-state (s)
  (let* ((macro (macro-function s))
         (fn (and (not macro) (fboundp s) (not (special-operator-p s)) (fdefinition s))))
    (vector macro
            fn
            (compiler-macro-function s)
            (and (not (constantp s)) (nth-value 1 (macroexpand-1 s)))
            (and (inline-p s) t)
            (and (not (fboundp `(setf ,s))) (setf-expander s))
            (and (special-p s) t)
            (and (constantp s) (not (keywordp s)) (boundp s))
            (and (type-defined-p s) t)
            (find-class s nil)
            (and (fboundp `(setf ,s)) (fdefinition `(setf ,s)))
            (and fn (mop-methods fn))
            (and (transform-p s) t))))

(defparameter +empty-state+ (make-array (length +kinds+) :initial-element nil))

;;; ------------------------------------------------------------------
;;; Recording a :PLAN build.

(defstruct recorder
  (old-packages '())
  (watched '())
  (states (make-hash-table :test 'eq))
  (features '())
  ;; (kind . name) -> ((index . component) ...), latest first. NAME is a
  ;; symbol, or a string for a package, or a keyword for a feature.
  (definers (make-hash-table :test 'equal)))

(defun package-key (p) (package-name p))

(defun snapshot (rec index component)
  "Note in REC every definition made since the last snapshot as made by
   COMPONENT at INDEX."
  (flet ((define (kind name)
           (push (cons index component)
                 (gethash (cons kind name) (recorder-definers rec)))))
    (dolist (p (list-all-packages))
      (unless (or (member p (recorder-old-packages rec))
                  (member p (recorder-watched rec)))
        (push p (recorder-watched rec))
        (define :package (package-key p))))
    (dolist (f *features*)
      (unless (member f (recorder-features rec))
        (push f (recorder-features rec))
        (define :feature f)))
    (let ((states (recorder-states rec)))
      (dolist (p (recorder-watched rec))
        (do-symbols (s p)
          (when (eq (symbol-package s) p)
            (let ((old (gethash s states +empty-state+))
                  (new (symbol-state s)))
              (unless (every #'eql old new)
                (loop for i below (length new)
                      unless (eql (aref old i) (aref new i))
                        do (when (aref new i) (define (aref +kinds+ i) s)))
                (setf (gethash s states) new)))))))))

;;; ------------------------------------------------------------------
;;; Reading a file for what it names.

(defstruct refs
  (symbols (make-hash-table :test 'eq))
  (packages (make-hash-table :test 'equal))
  (features (make-hash-table :test 'eq)))

(defvar *refs*)

(defun note-features (expr)
  (cond ((keywordp expr)
         (unless (member expr '(:and :or :not))
           (setf (gethash expr (refs-features *refs*)) t)))
        ((consp expr) (mapc #'note-features expr))))

(defun read-feature-conditional (stream subchar arg)
  (declare (ignore arg))
  (if *read-suppress*
      (progn (read stream t nil t) (read stream t nil t) (values))
      (let ((expr (let ((*package* (find-package :keyword))) (read stream t nil t))))
        (note-features expr)
        (if (eq (and (uiop:featurep expr) t) (char= subchar #\+))
            (read stream t nil t)
            (let ((*read-suppress* t)) (read stream t nil t) (values))))))

(defun auditing-readtable (&optional (base nil))
  (let ((rt (copy-readtable base)))
    (set-dispatch-macro-character #\# #\+ #'read-feature-conditional rt)
    (set-dispatch-macro-character #\# #\- #'read-feature-conditional rt)
    rt))

(defun note-package (designator)
  (let ((p (ignore-errors (find-package designator))))
    (when p (setf (gethash (package-key p) (refs-packages *refs*)) t))))

(defun note-symbols (form)
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (typecase x
                 (symbol
                  (let ((home (symbol-package x)))
                    (when (and home (not (keywordp x)))
                      (setf (gethash x (refs-symbols *refs*)) t)
                      (unless (or (eq home *package*)
                                  (eq (find-symbol (symbol-name x) *package*) x))
                        (note-package home)))))
                 (cons
                  (unless (gethash x seen)
                    (setf (gethash x seen) t)
                    (walk (car x))
                    (walk (cdr x))))
                 ((and vector (not string))
                  (unless (gethash x seen)
                    (setf (gethash x seen) t)
                    (map nil #'walk x))))))
      (walk form))))

(defun sets-readtable-p (form)
  "True when FORM, as its file was loaded, may have changed the readtable the
   rest of the file is read with: then it is evaluated again while reading."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (cond ((eq x '*readtable*) t)
                     ((and (consp x) (not (gethash x seen)))
                      (setf (gethash x seen) t)
                      (or (walk (car x)) (walk (cdr x)))))))
      (and (consp form)
           (walk (ignore-errors (macroexpand form)))))))

(defun package-form-p (form)
  (and (consp form) (symbolp (car form))
       (member (symbol-name (car form)) '("DEFPACKAGE" "DEFINE-PACKAGE") :test #'string=)))

(defun note-toplevel (form)
  "Follow IN-PACKAGE and IN-READTABLE, and note the packages a DEFPACKAGE
   names."
  (when (consp form)
    (let ((head (and (symbolp (car form)) (symbol-name (car form)))))
      (cond
        ((equal head "IN-PACKAGE")
         (let ((p (ignore-errors (find-package (second form)))))
           (when p (note-package p) (setf *package* p))))
        ((equal head "IN-READTABLE")
         (let ((finder (find-sym "FIND-READTABLE" "NAMED-READTABLES")))
           (when finder
             (let ((rt (ignore-errors (funcall finder (second form)))))
               (when rt (setf *readtable* (auditing-readtable rt)))))))
        ((member head '("DEFPACKAGE" "DEFINE-PACKAGE") :test #'equal)
         (dolist (option (cddr form))
           (when (consp option)
             (case (car option)
               ((:use :mix :reexport :use-reexport :mix-reexport)
                (mapc #'note-package (cdr option)))
               ((:import-from :shadowing-import-from)
                (note-package (second option)))
               (:local-nicknames
                (dolist (pair (cdr option))
                  (when (consp pair) (note-package (second pair)))))))))
        ((member head '("PROGN" "EVAL-WHEN") :test #'equal)
         (mapc #'note-toplevel (if (equal head "EVAL-WHEN") (cddr form) (cdr form))))))))

(defun file-references (file)
  "What FILE names: a REFS, and an error message if it could not be read to
   the end."
  (let ((*refs* (make-refs))
        (problem nil))
    (with-open-file (s file :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*package* (find-package :cl-user))
              (*readtable* (auditing-readtable))
              (*read-eval* t))
          (handler-case
              (loop for form = (read s nil s)
                    until (eq form s)
                    do (unless (package-form-p form) (note-symbols form))
                       (note-toplevel form)
                       (when (sets-readtable-p form)
                         (let ((before *readtable*))
                           (ignore-errors (eval form))
                           (unless (eq *readtable* before)
                             (setf *readtable* (auditing-readtable *readtable*))))))
            (error (e)
              (setf problem (or (ignore-errors (princ-to-string e)) "read error")))))))
    (values *refs* problem)))

;;; ------------------------------------------------------------------
;;; Plans.

(defun call-performing (fn o c)
  "Call FN, with *GENSYM-COUNTER* at 0 when (O . C) compiles a file."
  (if (and (typep o 'asdf:compile-op) (typep c 'asdf:cl-source-file))
      (let ((*gensym-counter* 0)) (funcall fn o c))
      (funcall fn o c)))

(defclass audit-plan (asdf:sequential-plan)
  ((order :initarg :order :initform :plan :reader plan-order)
   (log :initform nil :accessor plan-log)))

(defun source-action-p (o c)
  (and (typep c 'asdf:cl-source-file)
       (or (typep o 'asdf:compile-op) (typep o 'asdf:load-op))))

(defun plan-dependencies (actions)
  "Each action of ACTIONS -> the actions of ACTIONS it waits for, through the
   ones outside ACTIONS (already done)."
  (let ((in-plan (make-hash-table :test 'equal))
        (memo (make-hash-table :test 'equal))
        (deps (make-hash-table :test 'equal)))
    (dolist (a actions) (setf (gethash a in-plan) t))
    (labels ((direct (o c)
               (let ((acc '()))
                 (asdf::map-direct-dependencies
                  o c (lambda (do dc)
                        (let ((k (cons do dc)))
                          (if (gethash k in-plan)
                              (pushnew k acc :test 'equal)
                              (dolist (x (through do dc)) (pushnew x acc :test 'equal))))))
                 acc))
             (through (o c)
               (let ((key (cons o c)))
                 (multiple-value-bind (v hit) (gethash key memo)
                   (if hit v
                       (progn (setf (gethash key memo) nil)
                              (setf (gethash key memo) (direct o c))))))))
      (dolist (a actions)
        (setf (gethash a deps) (remove a (direct (car a) (cdr a)) :test 'equal))))
    deps))

(defun reachable-components (o c memo)
  (let ((key (cons o c)))
    (or (gethash key memo)
        (let ((set (make-hash-table :test 'eq)))
          (setf (gethash key memo) set)
          (asdf::map-direct-dependencies
           o c (lambda (do dc)
                 (setf (gethash dc set) t)
                 (maphash (lambda (k v) (setf (gethash k set) v))
                          (reachable-components do dc memo))))
          set))))

(defvar *recording* nil
  "During a :PLAN build: a plist of the recorder, the compile index of each
   file, and what each file's compile depends on.")

(defun pending-actions (plan)
  (loop for a in (asdf::plan-actions plan)
        for o = (asdf::action-operation a)
        for c = (asdf::action-component a)
        unless (asdf::action-already-done-p plan o c)
          collect (cons o c)))

(defun perform-recorded (plan actions)
  (let ((rec (getf *recording* :recorder))
        (compiled (getf *recording* :compiled))
        (closures (getf *recording* :closures))
        (memo (make-hash-table :test 'equal)))
    (loop for (o . c) in actions
          when (and (typep o 'asdf:compile-op) (typep c 'asdf:cl-source-file))
            do (setf (gethash c closures) (reachable-components o c memo)))
    (loop for (o . c) in actions
          for index = (incf (car (getf *recording* :counter)))
          do (when (and (typep o 'asdf:compile-op) (typep c 'asdf:cl-source-file))
               (setf (gethash c compiled) index))
             (call-performing #'asdf::perform-with-restarts o c)
             (asdf::mark-as-done plan o c)
             (when (and (typep o 'asdf:load-op) (typep c 'asdf:cl-source-file))
               (snapshot rec index c)))))

(defvar *failures* '()
  "(component message) for each compile or load of a file that failed and was
   done again later.")

(defun perform-reversed (plan actions)
  (let* ((deps (plan-dependencies actions))
         (order (coerce actions 'vector))
         (index (make-hash-table :test 'equal))
         (done (make-hash-table :test 'equal))
         (deferred (make-hash-table :test 'equal))
         (prefix 0)
         (remaining (copy-list actions)))
    (loop for a across order for i from 0 do (setf (gethash a index) i))
    (flet ((ready-p (a)
             (if (gethash a deferred)
                 (>= prefix (gethash a index))
                 (every (lambda (d) (gethash d done)) (gethash a deps)))))
      (loop while remaining do
        (let ((next nil))
          (dolist (a remaining)
            (when (and (ready-p a)
                       (or (null next) (> (gethash a index) (gethash next index))))
              (setf next a)))
          (unless next
            (error "audit: no action of ~D can proceed" (length remaining)))
          (setf remaining (remove next remaining :test 'eq :count 1))
          (destructuring-bind (o . c) next
            (let ((failure
                    (if (and (source-action-p o c) (not (gethash next deferred)))
                        (handler-case (progn (call-performing #'asdf:perform o c) nil)
                          (serious-condition (e)
                            (or (ignore-errors (princ-to-string e)) (string (type-of e)))))
                        (progn (call-performing #'asdf::perform-with-restarts o c) nil))))
              (cond (failure
                     (push (list c failure) *failures*)
                     (setf (gethash next deferred) t)
                     (push next remaining))
                    (t
                     (asdf::mark-as-done plan o c)
                     (setf (gethash next done) t)
                     (loop while (and (< prefix (length order))
                                      (gethash (aref order prefix) done))
                           do (incf prefix)))))))))))

(defmethod asdf:perform-plan ((plan audit-plan) &key)
  (let ((actions (pending-actions plan)))
    (ecase (plan-order plan)
      (:plan (perform-recorded plan actions))
      (:reverse (perform-reversed plan actions)))))

;;; ------------------------------------------------------------------
;;; The child: one build, written to a record file.

(defun component-key (c)
  "The system's name followed by the names of the path down to C."
  (let ((path (asdf:component-find-path c)))
    (cons (first path) (rest path))))

(defun fasl-of (c)
  (find "fasl" (asdf:output-files (asdf:make-operation 'asdf:compile-op) c)
        :key #'pathname-type :test #'equal))

(defun symbol-label (s)
  (format nil "~A:~A" (package-name (symbol-package s)) (symbol-name s)))

(defun name-label (name)
  (typecase name
    (keyword (format nil ":~A" (symbol-name name)))
    (symbol (symbol-label name))
    (t (string name))))

(defun candidates (rec closures compiled)
  "File key -> ((dependency-key kind name) ...), for each file compiled: what
   it names that a file it does not depend on defined before it, and that no
   file it depends on had defined. Second value:
   the same for files after it. Third: (file-key message) for each file that
   could not be read to the end."
  (let ((definers (recorder-definers rec))
        (out '())
        (later '())
        (unread '()))
    (maphash
     (lambda (c index)
       (let ((closure (gethash c closures))
             (found '())
             (after '()))
         (multiple-value-bind (refs problem) (file-references (asdf:component-pathname c))
           (when problem (push (list (component-key c) problem) unread))
           (flet ((consider (kind name)
                    (let* ((ds (gethash (cons kind name) definers))
                           (before (unless (find-if (lambda (d)
                                                      (and (< (car d) index)
                                                           (or (eq (cdr d) c)
                                                               (and closure (gethash (cdr d) closure)))))
                                                    ds)
                                     (find-if (lambda (d) (< (car d) index)) ds)))
                           (next (find-if (lambda (d) (> (car d) index)) ds :from-end t)))
                      (loop for (d . list) in (list (cons before 'found) (cons next 'after))
                            for g = (cdr d)
                            when (and g (not (eq g c)) (not (and closure (gethash g closure))))
                              do (let ((item (list (component-key g) kind (name-label name))))
                                   (if (eq list 'found)
                                       (pushnew item found :test #'equal)
                                       (pushnew item after :test #'equal)))))))
             (maphash (lambda (s v) (declare (ignore v))
                        (loop for kind across +kinds+ do (consider kind s)))
                      (refs-symbols refs))
             (maphash (lambda (p v) (declare (ignore v)) (consider :package p))
                      (refs-packages refs))
             (maphash (lambda (f v) (declare (ignore v)) (consider :feature f))
                      (refs-features refs))))
         (when found (push (cons (component-key c) (reverse found)) out))
         (when after (push (cons (component-key c) (reverse after)) later))))
     compiled)
    (values out later unread)))

(defun system-info (names)
  (loop for name in names
        for s = (asdf:find-system name nil)
        for asd = (and s (asdf:system-source-file s))
        when s
          collect (list name
                        (and asd (namestring asd))
                        (and asd (probe-file asd) (asdf-extra-deps:file-sha256 asd)))))

(defun source-files-of (plan-actions)
  (loop for (o . c) in plan-actions
        when (and (typep o 'asdf:compile-op) (typep c 'asdf:cl-source-file))
          collect c))

(defun child (&key system order cache record supplements (jobs 4))
  (setf uiop:*user-cache* (uiop:ensure-directory-pathname cache))
  (asdf:clear-output-translations)
  (dolist (f supplements) (asdf-extra-deps:load-supplements f))
  (setf uiop:*compile-file-failure-behaviour* :warn)
  (let* ((sys (asdf:find-system system))
         (files (asdf::with-asdf-session ()
                  (source-files-of
                   (pending-actions (asdf:make-plan nil (asdf:make-operation 'asdf:load-op) sys)))))
         (rec (make-recorder :old-packages (list-all-packages)
                             :features (copy-list *features*)))
         (*recording* (list :recorder rec
                            :compiled (make-hash-table :test 'eq)
                            :closures (make-hash-table :test 'eq)
                            :counter (list 0)))
         (*failures* '())
         (aborted nil)
         (t0 (get-internal-real-time)))
    (handler-case
        (handler-bind ((warning #'muffle-warning))
          (let ((*compile-verbose* nil) (*compile-print* nil))
            (ecase order
              ((:plan :reverse)
               (asdf:load-system sys :plan-class 'audit-plan :plan-options (list :order order)))
              (:parallel
               (unless (find-package :asdf-parallel)
                 #+dotcl (require "asdf-parallel")
                 #-dotcl (load (merge-pathnames "../asdf-parallel/asdf-parallel.lisp" *here*)))
               (funcall (find-sym "LOAD-SYSTEM" "ASDF-PARALLEL") sys :jobs jobs)
               (loop with compile = (asdf:make-operation 'asdf:compile-op)
                     for (what message) in (getf (symbol-value (find-sym "*STATISTICS*" "ASDF-PARALLEL"))
                                                 :retried)
                     for c = (find what files :test #'string=
                                              :key (lambda (c) (asdf:action-description compile c)))
                     do (push (list (or c what) message) *failures*))))))
      (serious-condition (e)
        (setf aborted (or (ignore-errors (princ-to-string e)) (string (type-of e))))))
    (let ((ms (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
          (findings nil) (later nil) (unread nil))
      (when (and (eq order :plan) (not aborted))
        (setf (values findings later unread)
              (candidates rec (getf *recording* :closures) (getf *recording* :compiled))))
      (with-open-file (out record :direction :output :if-exists :supersede
                                  :external-format :utf-8)
        (with-standard-io-syntax
          (let ((*print-readably* nil))
            (prin1 (list :implementation (lisp-implementation-type)
                         :order order :system system :ms ms
                         :cache (namestring uiop:*user-cache*)
                         :aborted aborted
                         :files (loop for c in files
                                      collect (list (component-key c)
                                                    (let ((f (fasl-of c))) (and f (namestring f)))))
                         :failed (loop for (c message) in (reverse *failures*)
                                       collect (list (if (typep c 'asdf:component)
                                                         (component-key c)
                                                         (princ-to-string c))
                                                     message))
                         :systems (system-info
                                   (remove-duplicates
                                    (mapcar (lambda (c) (asdf:component-name (asdf:component-system c)))
                                            files)
                                    :test #'equal))
                         :findings findings
                         :later later
                         :unread unread)
                   out)
            (terpri out)))))))

;;; ------------------------------------------------------------------
;;; The driver.

(defun default-setup ()
  "#+dotcl (require \"quicklisp\")
#-(or dotcl quicklisp)
(let ((s (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname))))
  (when (probe-file s) (load s)))")

(defun read-record (file)
  (with-open-file (s file :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)) (read s)))))

(defun run-child (lisp work tag system order supplements jobs setup)
  (let ((script (merge-pathnames (format nil "~A.lisp" tag) work))
        (record (merge-pathnames (format nil "~A.sexp" tag) work))
        (cache (merge-pathnames (format nil "~A-cache/" tag) work))
        (log (merge-pathnames (format nil "~A.log" tag) work)))
    (when (probe-file record) (delete-file record))
    (uiop:delete-directory-tree cache :validate t :if-does-not-exist :ignore)
    (with-open-file (s script :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* nil))
          (format s "(require \"asdf\")~%~A~%(load ~S)~%" setup (namestring (merge-pathnames "audit.lisp" *here*)))
          (format s "(asdf-extra-deps-audit::child :system ~S :order ~S :cache ~S :record ~S :supplements '~S :jobs ~D)~%"
                  system order (namestring cache) (namestring record)
                  (mapcar #'namestring supplements) jobs)
          (format s "(uiop:quit 0)~%"))))
    (uiop:run-program (append lisp (list (namestring script)))
                      :input nil :output (namestring log) :error-output :output
                      :if-output-exists :supersede :ignore-error-status t)
    (unless (probe-file record)
      (error "audit: the ~A build wrote no record; see ~A" order (namestring log)))
    (read-record record)))

(defun file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun differing-fasls (a b)
  "Keys of the files whose fasls differ between records A and B."
  (let ((b-files (make-hash-table :test 'equal))
        (out '()))
    (loop for (key fasl) in (getf b :files) do (setf (gethash key b-files) fasl))
    (if (search "dotcl" (string-downcase (getf a :implementation)))
        (let* ((text (uiop:run-program (append
                                        (list "dotnet" "run" (namestring *fasl-digest*) "--")
                                        (when (or (eq (getf a :order) :parallel)
                                                  (eq (getf b :order) :parallel))
                                          (list "--gensyms"))
                                        (list "--compare" (getf a :cache) (getf b :cache)))
                                       :output :string :error-output nil
                                       :ignore-error-status t))
               (differ (with-input-from-string (s text)
                         (loop for line = (read-line s nil)
                               while line
                               when (and (> (length line) 7) (string= "DIFFER " line :end2 7))
                                 collect (subseq line 7 (search " line " line :from-end t))))))
          (loop for (key fasl) in (getf a :files)
                when (and fasl
                          (member fasl differ
                                  :test (lambda (f rel)
                                          (let ((n (- (length f) (length rel))))
                                            (and (> n 0) (string= rel f :start2 n)
                                                 (char= #\/ (char f (1- n))))))))
                  do (push key out)))
        (loop for (key fasl) in (getf a :files)
              for other = (gethash key b-files)
              when (and fasl other (probe-file fasl) (probe-file other)
                        (not (equalp (file-bytes fasl) (file-bytes other))))
                do (push key out)))
    (reverse out)))

(defun file-designator (key)
  "How a supplement names the file KEY within its system."
  (let ((path (rest key))) (if (rest path) path (first path))))

(defun dependency-designator (file-key dep-key)
  "How the supplement of FILE-KEY's system names DEP-KEY."
  (if (equal (first file-key) (first dep-key))
      (file-designator dep-key)
      (list :system (first dep-key))))

(defun strong-p (use) (member (first use) *strong-kinds*))

(defun print-uses (uses stream)
  (loop for (kind name) in uses for i from 0
        do (if (< i 6)
               (format stream "    ~(~A~) ~A~%" kind name)
               (return (format stream "    ... ~D more~%" (- (length uses) 6))))))

(defun group-by-dependency (cands)
  "((dep-key (kind name) ...) ...) in the order the dependencies first appear."
  (let ((groups '()))
    (loop for (dep-key kind name) in cands
          do (let ((cell (assoc dep-key groups :test #'equal)))
               (unless cell (setf cell (list dep-key)) (push cell groups))
               (push (list kind name) (cdr cell))))
    (mapcar (lambda (cell) (cons (car cell) (reverse (cdr cell)))) (reverse groups))))

(defun audit (system &key lisp (work "audit/") (orders '(:plan :reverse)) baseline
                          (compare t) supplements (jobs 4) (setup (default-setup))
                          (stream *standard-output*) output)
  "Build SYSTEM in the two ORDERS, each in a fresh Lisp started with LISP, and
   report the dependencies its files use but do not declare. With BASELINE,
   first build it once more in the first order, and leave out the files whose
   fasls differ between two builds in the same order. COMPARE NIL leaves out
   the comparison of the fasls. SUPPLEMENTS are files
   the builds load with asdf-extra-deps first. Returns the supplement forms
   for what the report marks UNDECLARED, and writes them to OUTPUT if given."
  (let* ((work (uiop:ensure-directory-pathname (merge-pathnames work)))
         (supplements (mapcar #'truename supplements))
         (run (lambda (tag order)
                (let ((r (run-child lisp work tag system order supplements jobs setup)))
                  (when (getf r :aborted)
                    (error "audit: the ~(~A~) build of ~A stopped: ~A" order system (getf r :aborted)))
                  r)))
         (base (progn (ensure-directories-exist work)
                      (and baseline (funcall run "0-baseline" (first orders)))))
         (a (funcall run (format nil "1-~(~A~)" (first orders)) (first orders)))
         (b (funcall run (format nil "2-~(~A~)" (second orders)) (second orders)))
         (noise (and base compare (differing-fasls base a)))
         (differ (and compare (set-difference (differing-fasls a b) noise :test #'equal)))
         (failed (mapcar #'first (getf b :failed)))
         (findings (getf a :findings))
         (forms (make-hash-table :test 'equal)))
    (format stream "~&;; ~A: ~D files; ~(~A~) ~D ms, ~(~A~) ~D ms; ~D fasls differ~A, ~D files failed in the ~(~A~) order~%"
            system (length (getf a :files)) (getf a :order) (getf a :ms) (getf b :order) (getf b :ms)
            (length differ)
            (if base (format nil " (and ~D between two ~(~A~) builds)" (length noise) (getf a :order)) "")
            (length failed) (getf b :order))
    (loop for (file-key . cands) in findings
          for diff = (member file-key differ :test #'equal)
          for fail = (member file-key failed :test #'equal)
          for any-strong = (some (lambda (c) (strong-p (rest c))) cands)
          do (loop for (dep-key . uses) in (group-by-dependency cands)
                   for take = (or (some #'strong-p uses) (and fail (not any-strong)))
                   do (format stream "~:[WEAK      ~;UNDECLARED~] ~{~A~^/~} -> ~{~A~^/~}~:[~; [fasl differs]~]~:[~; [failed]~]~%"
                              take file-key dep-key diff fail)
                      (print-uses uses stream)
                      (when take
                        (push (dependency-designator file-key dep-key)
                              (gethash (cons (first file-key) (file-designator file-key)) forms)))))
    (dolist (key (union differ failed :test #'equal))
      (unless (assoc key findings :test #'equal)
        (format stream "~&UNEXPLAINED ~{~A~^/~}:~:[~; fasl differs~]~:[~; failed in the ~(~A~) order~]~%"
                key (member key differ :test #'equal) (member key failed :test #'equal) (getf b :order))
        (loop for (dep-key . uses) in (group-by-dependency (rest (assoc key (getf a :later) :test #'equal)))
              do (format stream "  uses what a later file defines: ~{~A~^/~}~%" dep-key)
                 (print-uses uses stream))))
    (when noise
      (format stream "~&;; differ between two ~(~A~) builds:~{ ~{~A~^/~}~}~%" (getf a :order) noise))
    (loop for (key message) in (getf a :unread)
          do (format stream "~&;; not read to the end: ~{~A~^/~}: ~A~%" key message))
    (let ((result '()))
      (loop for (name nil sha) in (getf a :systems)
            for entries = (loop for k being the hash-keys of forms using (hash-value deps)
                                when (equal (car k) name)
                                  collect (cons (cdr k) (reverse deps)))
            when entries
              do (push `(:system ,name :asd-sha256 (,sha)
                         :depends-on ,(sort entries #'string<
                                            :key (lambda (e) (princ-to-string (car e)))))
                       result))
      (setf result (nreverse result))
      (when output
        (with-open-file (s output :direction :output :if-exists :supersede :external-format :utf-8)
          (with-standard-io-syntax
            (let ((*print-readably* nil) (*print-right-margin* 100) (*print-case* :downcase))
              (format s ";;; ~A, ~A~%" system (lisp-implementation-type))
              (dolist (form result) (pprint form s) (terpri s))))))
      result)))
