;;;; Random integer form test, generation side.
;;;;
;;;; Generates N random integer forms with pfdietz's generator (ansi-test
;;;; random/random-int-form.lsp) and
;;;;   1. checks each one within this implementation (optimized COMPILE
;;;;      against the notinline / unoptimized version), and
;;;;   2. writes every case to CASES so that replay.lisp can recompute the
;;;;      results on another implementation (SBCL) and the two result files
;;;;      can be diffed.
;;;;
;;;; Parameters (set with --eval before --load):
;;;;   cl-user::*rf-n*      number of forms            (default 200)
;;;;   cl-user::*rf-size*   maximum form size          (default 100)
;;;;   cl-user::*rf-nvars*  number of input variables  (default 4)
;;;;   cl-user::*rf-out*    output directory           (default "out/random-forms/")
;;;;   cl-user::*rf-prune*  how many discrepancies to shrink (default 20)
;;;;   cl-user::*rf-extra*  true: also generate the shapes of extra.lisp
;;;;                        (floats, strings, arrays, multiple values, exits)
;;;;   cl-user::*rf-typed*  true: also generate the shapes of typed.lisp
;;;;                        (declared types, specialized arrays, local functions)
;;;;   cl-user::*rf-seed*   seed of the generator, an (unsigned-byte 32); NIL
;;;;                        (default) picks one. Written to self-summary.txt.
;;;;   cl-user::*rf-case-timeout*  seconds one case may take (default 300);
;;;;                        dotcl only, see RF-START-WATCHDOG
;;;;
;;;; Outputs in *rf-out*: cases.lsp, self-discrepancies.lsp (shrunk),
;;;; self-discrepancies-raw.lsp, self-summary.txt

(in-package :cl-user)

(defvar *rf-n* 200)
(defvar *rf-size* 100)
(defvar *rf-nvars* 4)
(defvar *rf-out* "out/random-forms/")
(defvar *rf-prune* 20)
(defvar *rf-case-timeout* 300)
(defvar *rf-extra* nil)
(defvar *rf-typed* nil)
(defvar *rf-seed* nil)

(load "test/random/load.lisp")
(when *rf-extra* (load "test/random/extra.lisp"))
(when *rf-typed* (load "test/random/typed.lisp"))

(in-package :cl-test)

(defvar *rf-safe*)

(defun rf-safety (decls) (or (second (assoc 'safety decls)) 1))

(defun rf-make-case (size nvars)
  "Same generation steps as TEST-RANDOM-INTEGER-FORM, but returns the case.
   The OPTIMIZE settings are drawn first: typed.lisp generates forms that
   signal an error only when both say safety 1 or more."
  (let* ((decls1 (make-random-optimize-settings))
         (decls2 (make-random-optimize-settings))
         (vars (subseq '(a b c d e f g h i j k l m n o p q r s u v w x y z)
                       0 nvars))
         (var-ranges (mapcar #'make-random-integer-range vars))
         (var-types (mapcar (lambda (range) `(integer ,(car range) ,(cadr range)))
                            var-ranges))
         (form (let ((*vars* (loop for v in vars
                                   for tp in var-types
                                   collect (make-var-desc :name v :type tp)))
                     (*random-int-form-blocks* nil)
                     (*random-int-form-catch-tags* nil)
                     (*go-tags* nil)
                     (*rf-safe* (and (> (rf-safety decls1) 0) (> (rf-safety decls2) 0))))
                 (with-random-integer-form-params
                   (make-random-integer-form (1+ (random size))))))
         (vals-list
           (loop repeat *random-vals-list-bound*
                 collect (mapcar (lambda (range)
                                   (random-from-interval (1+ (cadr range))
                                                         (car range)))
                                 var-ranges))))
    (list :vars vars :var-types var-types :vals-list vals-list
          :decls1 decls1
          :decls2 decls2
          :form form)))

(defun rf-seed-random-state (seed)
  "A random state determined by SEED: MT19937 seeded by init_genrand, in the
   627-word layout dotcl and SBCL print (#S(RANDOM-STATE :STATE ...))."
  #+sbcl (sb-ext:seed-random-state seed)
  #-sbcl
  (let ((words (list 0 #x9908B0DF 624))
        (s (ldb (byte 32 0) seed)))
    (loop for i from 0 below 624
          do (when (> i 0)
               (setf s (ldb (byte 32 0) (+ (* 1812433253 (logxor s (ash s -30))) i))))
             (push s words)
          finally (setf words (nreverse words)))
    (let ((*read-eval* nil))
      (read-from-string (format nil "#S(RANDOM-STATE :STATE ~S)" words)))))

(defun rf-write (obj stream)
  (let ((*package* (find-package :cl-test))
        (*print-circle* t) (*print-pretty* nil) (*print-readably* nil)
        (*print-length* nil) (*print-level* nil) (*print-base* 10)
        (*print-radix* nil))
    (prin1 obj stream)
    (terpri stream)))

(defun rf-kind-key (d)
  (let ((kind (getf d :kind))
        (cc (getf d :compiler-condition)))
    (cond (cc (list :compile-failure cc))
          ((eq (car kind) :different-results) (list :different-results))
          (t kind))))

(defvar *rf-current* nil
  "(INDEX CASE START-TIME) of the case being checked, NIL between cases.")

(defun rf-start-watchdog (out)
  "A case that never finishes (a compile that does not terminate, say) would
   stop the whole run with nothing written about it. On dotcl a watchdog thread
   writes such a case to self-hang.lsp and exits with status 3."
  #+dotcl
  (dotcl:make-thread
   (lambda ()
     (loop
       (sleep 5)
       (let ((cur *rf-current*))
         (when (and cur
                    (> (- (get-internal-real-time) (third cur))
                       (* cl-user::*rf-case-timeout* internal-time-units-per-second)))
           (with-open-file (s (merge-pathnames "self-hang.lsp" out)
                              :direction :output :if-exists :supersede)
             (rf-write (list* :index (first cur) (second cur)) s))
           (format t "~&random-forms: case ~D did not finish in ~D s (self-hang.lsp)~%"
                   (first cur) cl-user::*rf-case-timeout*)
           (finish-output)
           (dotcl:quit 3)))))
   :name "random-forms watchdog")
  #-dotcl
  (declare (ignore out)))

(defun rf-run ()
  (let* ((out (ensure-directories-exist
               (merge-pathnames cl-user::*rf-out* (truename "."))))
         (discrepancies '())
         (start (get-internal-real-time)))
    (unless cl-user::*rf-seed*
      (setf cl-user::*rf-seed* (random (expt 2 32) (make-random-state t))))
    (setf *random-state* (rf-seed-random-state cl-user::*rf-seed*))
    (rf-start-watchdog out)
    (with-open-file (cases (merge-pathnames "cases.lsp" out)
                           :direction :output :if-exists :supersede)
      (loop for i from 1 to cl-user::*rf-n*
            for c = (rf-make-case cl-user::*rf-size* cl-user::*rf-nvars*)
            do (rf-write (list* :index i c) cases)
               (finish-output cases)
               (setf *rf-current* (list i c (get-internal-real-time)))
               (let ((r (let ((*standard-output* (make-broadcast-stream)))
                          (test-int-form (getf c :form) (getf c :vars)
                                         (getf c :var-types) (getf c :vals-list)
                                         (getf c :decls1) (getf c :decls2)))))
                 (setf *rf-current* nil)
                 (dolist (d r) (push (list* :index i d) discrepancies)))
               (when (zerop (mod i 100))
                 (format t "~D " i) (finish-output))))
    (setf discrepancies (nreverse discrepancies))
    ;; Unshrunk too: the shrinker can turn a form into one that fails for a
    ;; different reason (or is no longer a valid form at all).
    (with-open-file (s (merge-pathnames "self-discrepancies-raw.lsp" out)
                       :direction :output :if-exists :supersede)
      (dolist (d discrepancies) (rf-write d s)))
    (let ((pruned (let ((*standard-output* (make-broadcast-stream)))
                    (ignore-errors
                     (prune-results
                      (subseq discrepancies 0
                              (min cl-user::*rf-prune*
                                   (length discrepancies))))))))
      (let ((minimal (or pruned discrepancies)))
        (with-open-file (s (merge-pathnames "self-discrepancies.lsp" out)
                           :direction :output :if-exists :supersede)
          (dolist (d minimal) (rf-write d s)))
        (dolist (d minimal)
          (format t "~&random-forms: seed ~D index ~D form ~S~%"
                  cl-user::*rf-seed* (getf d :index) (getf d :form)))))
    (let ((kinds (make-hash-table :test 'equal)))
      (dolist (d discrepancies) (incf (gethash (rf-kind-key d) kinds 0)))
      (with-open-file (s (merge-pathnames "self-summary.txt" out)
                         :direction :output :if-exists :supersede)
        (dolist (stream (list s *standard-output*))
          (format stream "~&random-forms: ~D forms, ~D self discrepancies (~,1F s)~%"
                  cl-user::*rf-n* (length discrepancies)
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second))
          (format stream "  seed ~D~@[, extra~]~@[, typed~]~%"
                  cl-user::*rf-seed* cl-user::*rf-extra* cl-user::*rf-typed*)
          (maphash (lambda (k n) (format stream "  ~5D ~S~%" n k)) kinds))))
    discrepancies))

(rf-run)
