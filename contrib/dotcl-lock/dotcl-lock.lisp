;;; dotcl-lock.lisp -- load the systems a lock file puts in package units.
;;;
;;;   (require "dotcl-lock")
;;;   (dotcl-lock:use-lock "dotcl-systems.lock")
;;;   (asdf:load-system "app")
;;;
;;; The lock file is one form:
;;;
;;;   (:dotcl-systems-lock
;;;    :format 1
;;;    :systems (("x" :asd "x-1/x.asd")
;;;              ("x" :asd "x-2/x.asd" :unit "x@2")
;;;              ("b" :asd "b/b.asd" :unit "x@2" :boundary t))
;;;    :units ((:name "x@2" :packages ("X" "X/*") :systems ("x"))))
;;;
;;; A system with :unit is defined and loaded in that unit. A boundary system
;;; is loaded in its unit but seen from outside as one leaf, so code outside
;;; depends on it by its plain name. Any other system a unit reaches is shared:
;;; it is loaded once, outside every unit. Relative :asd paths are relative to
;;; the lock file. Unit names are case-insensitive; a unit's packages print
;;; with the upcased name as prefix (X@2!X). A system the lock does not name is
;;; loaded as ASDF finds it, with a SYSTEM-NOT-IN-LOCK style warning.
;;; Experimental; the format may change.

(require "asdf")
(require "asdf-version-range")

(defpackage :dotcl-lock
  (:use :cl)
  (:export #:use-lock #:find-unit #:call-in-unit #:unit-name
           #:system-unit #:call-in-system-unit
           #:system-not-in-lock #:system-not-in-lock-name))

(in-package :dotcl-lock)

(defvar *unit* nil
  "The name of the unit this code runs in, NIL outside every unit. Each unit
binds it, so it follows the unit into threads the way the unit does.")

(defvar *lock* nil)
(defvar *units* (make-hash-table :test 'equal))
(defvar *global* nil
  "A unit that owns no names: entering it from inside another unit is the
nested way back to the global namespace.")

(defstruct (unit (:constructor %make-unit))
  name packages systems boundary object)

(defun asdf-var (name package)
  (or (uiop:find-symbol* name package nil)
      (error "dotcl-lock: no ~A::~A in this ASDF" package name)))

(defun unit-specials ()
  (list (asdf-var "*REGISTERED-SYSTEMS*" "ASDF/SYSTEM-REGISTRY")
        (asdf-var "*PRELOADED-SYSTEMS*" "ASDF/SYSTEM-REGISTRY")
        (asdf-var "*IMMUTABLE-SYSTEMS*" "ASDF/SYSTEM-REGISTRY")
        (asdf-var "*ASDF-SESSION*" "ASDF/SESSION")
        (asdf-var "*PACKAGE-INFERRED-SYSTEMS*" "ASDF/PACKAGE-INFERRED-SYSTEM")
        (asdf-var "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS")
        (asdf-var "*OUTPUT-TRANSLATIONS-PARAMETER*" "ASDF/OUTPUT-TRANSLATIONS")
        (asdf-var "*CENTRAL-REGISTRY*" "ASDF/SYSTEM-REGISTRY")
        (asdf-var "*SOURCE-REGISTRY*" "ASDF/SOURCE-REGISTRY")
        (asdf-var "*SOURCE-REGISTRY-PARAMETER*" "ASDF/SOURCE-REGISTRY")
        '*modules*
        '*unit*))

;;; Reading the lock

(defun read-lock (pathname)
  (let ((form (with-open-file (in pathname)
                (with-standard-io-syntax
                  (let ((*read-eval* nil)) (read in))))))
    (unless (and (consp form) (eq (car form) :dotcl-systems-lock))
      (error "dotcl-lock: ~A is not a lock file" pathname))
    (unless (eql (getf (cdr form) :format) 1)
      (error "dotcl-lock: ~A has format ~S, this reads 1"
             pathname (getf (cdr form) :format)))
    (cdr form)))

(defun resolve-asd (asd base)
  (let ((p (uiop:ensure-absolute-pathname (uiop:parse-native-namestring asd) base)))
    (unless (probe-file p)
      (error "dotcl-lock: ~A is not there; fetch the systems the lock names first" p))
    p))

;;; Units

(defun name-matches-p (name pattern)
  (let ((n (length pattern)))
    (if (and (plusp n) (char= (char pattern (1- n)) #\*))
        (and (>= (length name) (1- n)) (string= name pattern :end1 (1- n) :end2 (1- n)))
        (string= name pattern))))

(defun owned-package-p (unit name)
  (some (lambda (p) (name-matches-p (string-upcase name) (string-upcase p)))
        (unit-packages unit)))

(defun owned-system-p (unit name)
  (let ((primary (asdf:primary-system-name name)))
    (or (member primary (unit-systems unit) :test #'string=)
        (member primary (unit-boundary unit) :test #'string=))))

(defun copy-table-without (table owned-p)
  (when table
    (let ((new (make-hash-table :test (hash-table-test table))))
      (maphash (lambda (k v) (unless (funcall owned-p k) (setf (gethash k new) v))) table)
      new)))

(defun unit-output-translation (name global)
  (lambda (path absolute-source)
    (declare (ignore absolute-source))
    (let* ((sym (asdf-var "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))
           (out (progv (list sym) (list global) (asdf:apply-output-translations path))))
      (make-pathname :directory (append (pathname-directory out) (list (format nil "unit-~A" name)))
                     :defaults out))))

(defun initial-values (unit)
  (flet ((v (name package) (symbol-value (asdf-var name package))))
    (let ((own-system (lambda (n) (owned-system-p unit n)))
          (own-package (lambda (n) (owned-package-p unit n)))
          (translations (progn (asdf:ensure-output-translations)
                               (v "*OUTPUT-TRANSLATIONS*" "ASDF/OUTPUT-TRANSLATIONS"))))
      (list (copy-table-without (v "*REGISTERED-SYSTEMS*" "ASDF/SYSTEM-REGISTRY") own-system)
            (copy-table-without (v "*PRELOADED-SYSTEMS*" "ASDF/SYSTEM-REGISTRY") own-system)
            (copy-table-without (v "*IMMUTABLE-SYSTEMS*" "ASDF/SYSTEM-REGISTRY") own-system)
            nil
            (copy-table-without (v "*PACKAGE-INFERRED-SYSTEMS*" "ASDF/PACKAGE-INFERRED-SYSTEM")
                                own-package)
            (list (list (list t (unit-output-translation (unit-name unit) translations))))
            nil
            nil
            (make-hash-table :test 'equal)
            nil
            (remove-if (lambda (m) (owned-package-p unit m)) *modules*)
            (unit-name unit)))))

(defun global-values ()
  (asdf:ensure-output-translations)
  (append (mapcar #'symbol-value (butlast (unit-specials))) (list nil)))

(defun make-unit (name packages values)
  (let ((object (dotcl::%make-package-unit name (format nil "~{~A~^,~}" packages))))
    (progv (unit-specials) values
      (dolist (s (unit-specials))
        (dotcl::%add-package-unit-special object s)))
    object))

(defun ensure-global-unit ()
  (or *global*
      (setf *global*
            (let ((object (make-unit "GLOBAL" '() (global-values))))
              (%make-unit :name nil :object object)))))

(defun find-unit (name)
  (if name
      (or (gethash (string-upcase name) *units*) (error "dotcl-lock: no unit ~S" name))
      (ensure-global-unit)))

(defun call-in-unit (name thunk)
  "Call THUNK in the unit named NAME (NIL: outside every unit)."
  (if (equal (and name (string-upcase name)) *unit*)
      (funcall thunk)
      (dotcl::%call-in-package-unit (unit-object (find-unit name)) thunk)))

(defun system-unit (name)
  "The name of the unit the lock loads system NAME in, NIL when outside every
unit or not in the lock. An error when the lock has it in more than one place."
  (let* ((primary (asdf:primary-system-name (asdf:coerce-name name)))
         (units (remove-duplicates
                 (loop for e in (getf *lock* :systems)
                       when (string= (first e) primary) collect (getf (rest e) :unit))
                 :test #'equal)))
    (when (rest units)
      (error "dotcl-lock: system ~S is in more than one place (~{~:[global~;~:*~A~]~^, ~})"
             primary units))
    (first units)))

(defun call-in-system-unit (system thunk)
  "Call THUNK in the unit the lock loads SYSTEM in."
  (call-in-unit (system-unit system) thunk))

;;; Finding systems

(defclass proxy-system (asdf:system)
  ((target-unit :initarg :target-unit :reader target-unit))
  (:documentation "A system that lives in another unit (or outside every unit),
seen here as one leaf: loading it loads the real one there."))

(deftype load-operation ()
  "What LOAD-SYSTEM performs: LOAD-OP, or LOAD-SOURCE-OP where nothing is compiled."
  '(or asdf:load-op asdf:load-source-op))

(defmethod asdf:perform ((o asdf:operation) (c proxy-system))
  (when (typep o 'load-operation)
    (let ((name (asdf:component-name c)))
      (call-in-unit (target-unit c) (lambda () (asdf:load-system name))))))

(defmethod asdf::compute-action-stamp (plan (o asdf:operation) (c proxy-system) &key just-done)
  "The real system's stamp as a plan there predicts it, so what depends on a
proxy is exactly as up to date as it would be on the real system."
  (if (typep o 'load-operation)
      (let ((name (asdf:component-name c))
            (class (type-of o)))
        (call-in-unit (target-unit c)
                      (lambda ()
                        (asdf/session::call-with-asdf-session
                         (lambda ()
                           (let* ((s (asdf:find-system name))
                                  (op (asdf:make-operation class))
                                  (status (asdf::action-status (asdf::make-plan nil op s) op s)))
                             (values (asdf::status-stamp status)
                                     (asdf::status-done-p status))))))))
      (call-next-method)))

(defun dependency-name (spec)
  (cond ((or (stringp spec) (symbolp spec)) (asdf:coerce-name spec))
        ((and (consp spec) (eq (first spec) :version)) (asdf:coerce-name (second spec)))
        ((and (consp spec) (eq (first spec) :feature))
         (and (uiop:featurep (second spec)) (dependency-name (third spec))))))

(defun shared-closure (unit name)
  "The systems outside UNIT that NAME reaches through UNIT's own systems."
  (let ((seen '()) (shared '()))
    (labels ((walk (n)
               (unless (member n seen :test #'string=)
                 (push n seen)
                 (dolist (spec (asdf:system-depends-on (asdf:find-system n)))
                   (let ((d (dependency-name spec)))
                     (when d
                       (if (owned-system-p unit (asdf:primary-system-name d))
                           (walk d)
                           (pushnew d shared :test #'string=))))))))
      (walk name))
    (reverse shared)))

(defmethod asdf:component-depends-on ((o asdf:operation) (c proxy-system))
  "Outside its unit, a proxy first loads the shared systems the real one uses,
so they are loaded here, once, in this plan's order."
  (let ((unit (target-unit c)))
    (if (and unit (typep o 'load-operation))
        (let ((shared (call-in-unit unit (lambda ()
                                           (shared-closure (find-unit unit)
                                                           (asdf:component-name c))))))
          (if shared
              (cons (cons (type-of o) shared) (call-next-method))
              (call-next-method)))
        (call-next-method))))

(defvar *proxies* (make-hash-table :test 'equal))

(defun proxy (name target)
  (let ((key (list *unit* target name)))
    (or (gethash key *proxies*)
        (setf (gethash key *proxies*)
              (make-instance 'proxy-system :name name :target-unit target)))))

(defun lock-entry (name unit)
  (let ((primary (asdf:primary-system-name name)))
    (find-if (lambda (e) (and (string= (first e) primary)
                              (equal (getf (rest e) :unit) unit)))
             (getf *lock* :systems))))

(defun boundary-unit (name)
  (let ((e (find-if (lambda (e) (and (string= (first e) (asdf:primary-system-name name))
                                     (getf (rest e) :boundary)))
                    (getf *lock* :systems))))
    (and e (getf (rest e) :unit))))

(define-condition system-not-in-lock (style-warning)
  ((name :initarg :name :reader system-not-in-lock-name))
  (:report (lambda (c s)
             (format s "dotcl-lock: system ~S is not in the lock; loading it from wherever ASDF finds it"
                     (system-not-in-lock-name c)))))

(defvar *warned-not-in-lock* '())

(defun note-not-in-lock (name)
  (let ((primary (asdf:primary-system-name name)))
    (unless (or (find primary (getf *lock* :systems) :key #'first :test #'string=)
                (asdf:registered-system primary)
                (member primary *warned-not-in-lock* :test #'string=))
      (push primary *warned-not-in-lock*)
      (warn 'system-not-in-lock :name primary))))

(defun sysdef-lock-search (name)
  (when *lock*
    (let ((entry (lock-entry name *unit*)))
      (cond (entry (getf (rest entry) :asd))
            ((null *unit*)
             (let ((u (boundary-unit name)))
               (if u (proxy name u) (progn (note-not-in-lock name) nil))))
            ((not (owned-system-p (find-unit *unit*) name))
             (proxy name nil))))))

(defun unit-only-system-p (name)
  "True outside every unit for a system the lock loads only inside units."
  (and *lock* (null *unit*)
       (let* ((primary (asdf:primary-system-name name))
              (entries (remove primary (getf *lock* :systems) :key #'first :test-not #'string=)))
         (and entries
              (every (lambda (e) (getf (rest e) :unit)) entries)
              (notany (lambda (e) (getf (rest e) :boundary)) entries)))))

(defmethod asdf:find-system :around ((name string) &optional (error-p t))
  "Outside every unit, a system the lock loads only in units is not there,
even when another search (a source registry over the same tree) would find
some version of it."
  (if (unit-only-system-p name)
      (when error-p (error 'asdf:missing-component :requires name))
      (call-next-method)))

(defun use-lock (pathname)
  "Read the lock file at PATHNAME, make its units, and have ASDF find the
systems it names there. Call it once, before loading any system it names."
  (when *lock* (error "dotcl-lock: a lock is already in use"))
  (let* ((pathname (merge-pathnames pathname))
         (base (uiop:pathname-directory-pathname pathname))
         (lock (read-lock pathname)))
    (setf (getf lock :systems)
          (mapcar (lambda (e)
                    (let ((unit (getf (rest e) :unit)))
                      (list* (first e) :asd (resolve-asd (getf (rest e) :asd) base)
                             :unit (and unit (string-upcase unit))
                             (rest e))))
                  (getf lock :systems)))
    (setf *global* nil)
    (ensure-global-unit)
    (dolist (spec (getf lock :units))
      (let* ((name (string-upcase (getf spec :name)))
             (unit (%make-unit :name name
                               :packages (getf spec :packages)
                               :systems (getf spec :systems)
                               :boundary (loop for e in (getf lock :systems)
                                               when (and (equal (getf (rest e) :unit) name)
                                                         (getf (rest e) :boundary))
                                                 collect (first e)))))
        (setf (unit-object unit) (make-unit name (unit-packages unit) (initial-values unit))
              (gethash name *units*) unit)))
    (setf *lock* lock)
    (pushnew 'sysdef-lock-search asdf:*system-definition-search-functions*)
    pathname))
