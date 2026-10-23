;;; resolve.lisp -- write a dotcl-lock lock file from directories of systems.
;;;
;;;   (load "contrib/dotcl-lock/resolve.lisp")
;;;   (dotcl-lock-resolve:resolve '("app")
;;;                               '("./" "/path/to/dist/")
;;;                               :output "dotcl-systems.lock")
;;;
;;; Every .asd under the source directories offers the systems it defines, at
;;; the version its primary system declares; a directory may hold several
;;; versions of one system. Versions are chosen as Cargo does: one version for
;;; each compatible range (the leading components up to the first nonzero one,
;;; so 1.x, 2.x, 0.3.x), the highest that every dependency on it allows; a
;;; second version only when a dependency cannot use the first. The version
;;; most systems use is loaded globally, the others in package units, with the
;;; systems that use them. The .asd files are read, not loaded, under
;;; :FEATURES. Runs on any Lisp with ASDF.

(require "asdf")
(unless (find-package :asdf-version-range)
  #+dotcl (require "asdf-version-range")
  #-dotcl (load (merge-pathnames "../asdf-version-range/asdf-version-range.lisp"
                                 (or *load-truename* *compile-file-truename*))))

(defpackage :dotcl-lock-resolve
  (:use :cl)
  (:import-from :asdf-version-range
                #:range-satisfies-p #:check-range #:version-compare
                #:compatible-key #:compatible-key-string)
  (:export #:resolve #:resolve-failed #:resolve-failed-system
           #:resolve-failed-requirements #:read-sources))

(in-package :dotcl-lock-resolve)

(defparameter *single* '("asdf" "uiop")
  "Systems an image can hold only one version of.")

;;; Reading .asd files

(defstruct release asd version systems)  ; systems: (name . depends-on) for each defsystem

(defun primary (name) (asdf:primary-system-name name))

(defun read-forms (pathname)
  "The top-level forms of PATHNAME up to the first it cannot read, read into
a scratch package."
  (let ((*package* (make-package (format nil "DOTCL-LOCK-RESOLVE-~D" (random 1000000000))
                                 :use '(:cl))))
    (unwind-protect
         (with-open-file (in pathname)
           (loop for form = (handler-case (read in nil in) (error () in))
                 until (eq form in)
                 collect form))
      (delete-package *package*))))

(defun version-of (form asd)
  (if (and (consp form) (member (car form) '(:read-file-form :read-file-line)))
      (destructuring-bind (path &key (at 0)) (rest form)
        (let ((file (merge-pathnames path asd)))
          (if (eq (car form) :read-file-form)
              (uiop:read-file-form file :at at)
              (uiop:read-file-line file :at at))))
      form))

(defun read-asd (asd features)
  (let ((systems '()) (version nil))
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*features* features))
        (dolist (form (read-forms asd))
          (when (and (consp form) (symbolp (car form)) (string= (car form) "DEFSYSTEM"))
            (let ((name (asdf:coerce-name (second form)))
                  (options (cddr form)))
              (push (cons name (append (getf options :defsystem-depends-on)
                                       (getf options :depends-on)))
                    systems)
              (when (string= name (primary name))
                (setf version (version-of (getf options :version) asd))))))))
    (when systems
      (make-release :asd asd :version version :systems (nreverse systems)))))

(defun read-sources (directories &key (features *features*))
  "A table from primary system name to the releases the .asd files under
DIRECTORIES define, highest version first."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (dir directories)
      (dolist (asd (directory (merge-pathnames "**/*.asd" (uiop:ensure-directory-pathname dir))))
        (let ((r (read-asd asd features)))
          (when r
            (push r (gethash (primary (car (first (release-systems r)))) table))))))
    (maphash (lambda (k rs)
               (setf (gethash k table)
                     (sort rs (lambda (a b)
                                (plusp (version-compare (or (release-version a) "0")
                                                        (or (release-version b) "0")))))))
             table)
    table))

;;; Dependencies

(defun dependency (spec features)
  "(name . range) for one :depends-on entry, or NIL if it names no system."
  (cond ((or (stringp spec) (symbolp spec)) (cons (asdf:coerce-name spec) nil))
        ((not (consp spec)) nil)
        ((eq (first spec) :version)
         (cons (asdf:coerce-name (second spec)) (check-range (third spec))))
        ((eq (first spec) :feature)
         (and (uiop:featurep (second spec) features)
              (dependency (third spec) features)))))

(defun release-dependencies (release name features)
  (let ((entry (assoc name (release-systems release) :test #'string=)))
    (loop for spec in (rest entry)
          for d = (dependency spec features)
          when d collect d)))

(defun allows-p (release range)
  (or (null range)
      (and (release-version release) (range-satisfies-p (release-version release) range))))

;;; Solving. An activation is (primary . release). A requirement is
;;; (from-activation name range path).

(define-condition resolve-failed (error)
  ((system :initarg :system :reader resolve-failed-system)
   (requirements :initarg :requirements :reader resolve-failed-requirements))
  (:report (lambda (c s)
             (format s "No set of versions satisfies every dependency on ~S:~{~%  ~{~{~A~^ -> ~} wants ~S~}~}"
                     (resolve-failed-system c)
                     (resolve-failed-requirements c)))))

(defvar *failure* nil)

(defun compatible-p (a b)
  (equal (compatible-key (or (release-version a) "0"))
         (compatible-key (or (release-version b) "0"))))

(defun note-failure (name range path activations edges)
  (let ((wanted (append (loop for (nil to r p) in edges
                              when (string= (car to) name) collect (list p r))
                        (list (list path range)))))
    (when (or (null *failure*) (> (length wanted) (length (getf *failure* :wanted))))
      (setf *failure* (list :name name :wanted wanted :activations activations)))))

(defun solve (queue activations edges expanded sources features)
  "Activations and edges once every requirement in QUEUE is met, or NIL."
  (when (null queue)
    (return-from solve (values activations edges t)))
  (destructuring-bind ((from name range path) &rest rest) queue
    (let* ((p (primary name))
           (active (remove p activations :key #'car :test-not #'string=))
           (single (member p *single* :test #'string=))
           (candidates (gethash p sources)))
      (unless candidates
        (return-from solve (solve rest activations edges expanded sources features)))
      (flet ((try (activation new-p)
               (let* ((key (cons activation name))
                      (more (unless (member key expanded :test #'equal)
                              (loop for (d . r) in (release-dependencies (cdr activation) name features)
                                    collect (list activation d r (append path (list d)))))))
                 (multiple-value-bind (acts eds ok)
                     (solve (append rest more)
                            (if new-p (append activations (list activation)) activations)
                            (cons (list from activation range path) edges)
                            (if more (cons key expanded) expanded)
                            sources features)
                   (when ok (return-from solve (values acts eds t)))))))
        (dolist (a active)
          (when (allows-p (cdr a) range)
            (try a nil)))
        (unless (and single active)
          (dolist (r candidates)
            (when (and (allows-p r range)
                       (notany (lambda (a) (compatible-p r (cdr a))) active))
              (try (cons p r) t))))
        (note-failure p range path active edges)
        nil))))

;;; Placing activations in units

(defun choose-global (activations edges)
  "The activation of each name loaded outside every unit: the one most
systems depend on, the highest version on a tie."
  (let ((global '()))
    (dolist (a activations)
      (let ((rivals (remove (car a) activations :key #'car :test-not #'string=)))
        (flet ((users (x) (length (remove-duplicates
                                   (loop for (from to) in edges when (eq to x) collect from)))))
          (when (eq a (first (sort (copy-list rivals)
                                   (lambda (x y)
                                     (let ((ux (users x)) (uy (users y)))
                                       (or (> ux uy)
                                           (and (= ux uy)
                                                (plusp (version-compare
                                                        (or (release-version (cdr x)) "0")
                                                        (or (release-version (cdr y)) "0"))))))))))
            (push a global)))))
    global))

(defun needs (activation edges global &optional seen)
  "The activations outside the global set that ACTIVATION's code is compiled
against: its own, and those its non-global dependencies need."
  (unless (member activation seen)
    (let ((own (unless (member activation global) (list activation))))
      (remove-duplicates
       (append own
               (loop for (from to) in edges
                     when (and (eq from activation) (not (member to global)))
                       append (needs to edges global (cons activation seen))))))))

(defun activation-label (a)
  (format nil "~A@~A" (car a) (compatible-key-string (or (release-version (cdr a)) "0"))))

(defun unit-name (set)
  (format nil "~{~A~^+~}" (sort (mapcar #'activation-label set) #'string<)))

(defun package-names (directory)
  "The packages the top-level forms of the Lisp files under DIRECTORY define."
  (let ((names '()))
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (dolist (file (directory (merge-pathnames "**/*.lisp" directory)))
          (dolist (form (read-forms file))
            (when (and (consp form) (symbolp (car form))
                       (member (symbol-name (car form)) '("DEFPACKAGE" "DEFINE-PACKAGE")
                               :test #'string=))
              (pushnew (string (second form)) names :test #'string=)
              (loop for option in (cddr form)
                    when (and (consp option) (eq (car option) :nicknames))
                      do (dolist (n (cdr option)) (pushnew (string n) names :test #'string=))))))))
    names))

(defun lock-path (asd base)
  (let ((relative (and base (enough-namestring asd base))))
    (if (and relative (not (uiop:absolute-pathname-p relative)))
        relative
        (namestring asd))))

(defun write-lock (roots activations edges global base stream)
  (let* ((placed (loop for a in activations
                       for need = (needs a edges global)
                       collect (list a need)))
         (units (remove-duplicates
                 (loop for (a need) in placed
                       when (and need (member a global)) collect need)
                 :test (lambda (x y) (and (subsetp x y) (subsetp y x))))))
    (flet ((entry (a &rest more)
             (list* (car a) :asd (lock-path (release-asd (cdr a)) base)
                    (append (and (release-version (cdr a)) (list :version (release-version (cdr a))))
                            more))))
      (let ((systems
              (append
               (loop for (a need) in placed
                     when (and (member a global) (null need)) collect (entry a))
               (loop for set in units
                     append (loop for a in set collect (entry a :unit (unit-name set))))
               (loop for (a need) in placed
                     when (and (member a global) need)
                       collect (entry a :unit (unit-name need) :boundary t))))
            (unit-specs
              (loop for set in units
                    collect
                    (let ((owned (remove-duplicates (mapcar #'car set) :test #'string=)))
                      (list :name (unit-name set)
                            :packages (sort (remove-duplicates
                                             (loop for a in activations
                                                   when (member (car a) owned :test #'string=)
                                                     append (package-names
                                                             (uiop:pathname-directory-pathname
                                                              (release-asd (cdr a)))))
                                             :test #'string=)
                                            #'string<)
                            :systems owned
                            :why (loop for (from to range) in edges
                                       when (member (car to) owned :test #'string=)
                                         collect (list (if from (car from) :root) (car to)
                                                       (or range :any))))))))
        (with-standard-io-syntax
          (let ((*print-case* :downcase) (*print-readably* nil))
            (format stream "(:dotcl-systems-lock~% :format 1~% :roots ~S~% :systems~% (~{~S~^~%  ~})~% :units~% (~{~S~^~%  ~}))~%"
                    roots systems unit-specs)))))))

(defun resolve (roots directories &key output (features *features*) (single *single*))
  "Choose a version of every system ROOTS depend on from the .asd files under
DIRECTORIES and write the lock to OUTPUT (a pathname, or a stream; NIL
returns the text). SINGLE names the systems that may not have two versions.
Signals RESOLVE-FAILED when no choice satisfies every dependency."
  (let* ((roots (mapcar #'asdf:coerce-name roots))
         (sources (read-sources directories :features features))
         (*single* single)
         (*failure* nil))
    (multiple-value-bind (activations edges ok)
        (solve (loop for r in roots collect (list nil r nil (list r)))
               '() '() '() sources features)
      (unless ok
        (error 'resolve-failed :system (getf *failure* :name)
                               :requirements (getf *failure* :wanted)))
      (let ((edges (reverse edges))
            (global (choose-global activations edges)))
        (flet ((emit (stream base) (write-lock roots activations edges global base stream)))
          (etypecase output
            (null (with-output-to-string (s) (emit s nil)))
            (stream (emit output nil))
            ((or string pathname)
             (let ((path (merge-pathnames output)))
               (ensure-directories-exist path)
               (with-open-file (s path :direction :output :if-exists :supersede)
                 (emit s (uiop:pathname-directory-pathname path)))
               path))))))))
