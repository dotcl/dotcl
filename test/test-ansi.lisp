;;; ANSI test runner for dotcl
;;; Loads rt framework + ansi-aux.lsp + cons tests

(in-package :cl-user)

;;; Define compile-and-load / compile-and-load* as just load for dotcl
;;; (dotcl doesn't produce .fasl files — always loads source directly)
(defvar *aux-dir* (pathname "ansi-test/auxiliary/"))

(defun compile-and-load (path &key force)
  (declare (ignore force))
  (let* ((s (namestring (pathname path)))
         ;; Translate ANSI-TESTS:AUX;foo.lsp -> ansi-test/auxiliary/foo.lsp
         (p (cond
             ((and (>= (length s) 15)
                   (string= (string-upcase (subseq s 0 15)) "ANSI-TESTS:AUX;"))
              (pathname (concatenate 'string "ansi-test/auxiliary/"
                                     (subseq s 15))))
             (t (let ((p2 (pathname s)))
                  (if *load-pathname*
                      (merge-pathnames p2 *load-pathname*)
                      p2))))))
    (load p)))

(defun compile-and-load* (path &key force)
  "Load a file relative to *aux-dir* (ansi-test/auxiliary/).
   Calls load directly to avoid compile-and-load's *load-pathname* merge."
  (declare (ignore force))
  (load (merge-pathnames path *aux-dir*)))

;;; Ensure sandbox directory exists BEFORE loading rt.lsp.
;;; rt.lsp computes *sandbox-path* = (ignore-errors (truename #P"sandbox/")) at load time.
;;; If sandbox/ does not exist yet, truename fails and *sandbox-path* = NIL, causing
;;; *default-pathname-defaults* = NIL during do-tests, which breaks MAKE-PATHNAME tests.
(ensure-directories-exist "sandbox/dummy.txt")

;;; Load the RT test framework
(format t "~%=== Loading RT framework ===~%")
(load "ansi-test/rt-package.lsp")
(load "ansi-test/rt.lsp")

;;; Load the CL-TEST package
(format t "=== Loading CL-TEST package ===~%")
(load "ansi-test/cl-test-package.lsp")

;;; Category range tracking: (name start-idx end-idx) — filled during loads
(defvar *ansi-category-ranges* nil)
(defun %ansi-cat-start () (length (cdr regression-test::*entries*)))
(defun %ansi-cat-end (name start)
  (push (list name start (length (cdr regression-test::*entries*)))
        *ansi-category-ranges*))

;;; Switch to CL-TEST for all subsequent loads (matching gclload1.lsp)
(in-package :cl-test)

;;; Load ansi-aux-macros BEFORE universe.lsp — the gitlab ansi-test repo
;;; shadows handler-case/handler-bind in :cl-test and provides replacements
;;; in ansi-aux-macros.lsp. universe.lsp uses handler-case, so the macros
;;; must be available by the time it loads.
(format t "=== Loading ansi-aux-macros ===~%")
(compile-and-load "ANSI-TESTS:AUX;ansi-aux-macros.lsp")

;;; Load universe.lsp (provides *universe*, *condition-types*, etc.)
(format t "=== Loading universe ===~%")
(load "ansi-test/universe.lsp")

;;; Override CLTEST: to map to CWD (not sandbox/).
;;; universe.lsp sets CLTEST: -> CWD/sandbox/, but our DPD = CWD during do-tests,
;;; so CLTEST:probe-file.txt must equal (truename #p"probe-file.txt") = CWD/probe-file.txt.
(setf (logical-pathname-translations "CLTEST")
  `(("**;*.*.*" ,(make-pathname :directory (pathname-directory (truename #p"./"))
                                :name :wild :type :wild :version :wild))))

;;; Load cl-symbol-names.lsp (provides *cl-symbol-names*, *cl-non-function-macro-special-operator-symbols*, etc.)
(format t "=== Loading cl-symbol-names ===~%")
(load "ansi-test/cl-symbol-names.lsp")

;;; Load ansi-aux.lsp (the full helper library)
(format t "=== Loading ansi-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;ansi-aux.lsp")

;;; Load cons auxiliary
(format t "=== Loading random-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;random-aux.lsp")

(format t "=== Loading cons-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;cons-aux.lsp")

;;; Load string auxiliary
(format t "=== Loading string-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;string-aux.lsp")

;;; Load cons tests via load.lsp (which uses *load-pathname* for relative loads)
(format t "=== Loading cons tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/cons/load.lsp")
  (cl-user::%ansi-cat-end "cons" %s))

;;; Load strings tests
(format t "=== Loading strings tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/strings/load.lsp")
  (cl-user::%ansi-cat-end "strings" %s))

;;; Load sequences tests
(format t "=== Loading sequences tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/sequences/load.lsp")
  (cl-user::%ansi-cat-end "sequences" %s))

;;; Load types auxiliary (needed by data-and-control-flow)
(format t "=== Loading types-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;types-aux.lsp")

;;; Load data-and-control-flow tests
(format t "=== Loading data-and-control-flow tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/data-and-control-flow/load.lsp")
  (cl-user::%ansi-cat-end "data-and-control-flow" %s))

;;; Load hash-tables tests
(format t "=== Loading hash-tables tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/hash-tables/load.lsp")
  (cl-user::%ansi-cat-end "hash-tables" %s))

;;; Load symbols tests
(format t "=== Loading symbols tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/symbols/load.lsp")
  (cl-user::%ansi-cat-end "symbols" %s))

;;; Load arrays tests
(format t "=== Loading arrays tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/arrays/load.lsp")
  (cl-user::%ansi-cat-end "arrays" %s))

;;; Create fixture files needed by types-and-classes tests
(unless (probe-file "class-precedence-lists.txt")
  (with-open-file (s "class-precedence-lists.txt" :direction :output :if-does-not-exist :create)
    (declare (ignore s))))

;;; Load types-and-classes tests
(format t "=== Loading types-and-classes tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/types-and-classes/load.lsp")
  (cl-user::%ansi-cat-end "types-and-classes" %s))

;;; Load characters tests
(format t "=== Loading characters tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/characters/load.lsp")
  (cl-user::%ansi-cat-end "characters" %s))

;;; Create fixture files needed by pathnames tests (in cwd AND sandbox/)
(dolist (name '("file-namestring.txt" "directory-namestring.txt"
                "host-namestring.txt" "enough-namestring.txt"
                "pathname.txt" "logical-pathname.txt"))
  (unless (probe-file name)
    (with-open-file (s name :direction :output :if-does-not-exist :create)
      (declare (ignore s))))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (unless (probe-file sandbox-name)
      (with-open-file (s sandbox-name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; Load pathnames tests
(format t "=== Loading pathnames tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/pathnames/load.lsp")
  (cl-user::%ansi-cat-end "pathnames" %s))

;;; Create fixture files needed by files tests (they need to exist at *default-pathname-defaults*)
(dolist (name '("truename.txt" "probe-file.txt" "file-author.txt"
                "file-write-date.txt" "ensure-directories-exist.txt" "file-error.txt"))
  (unless (probe-file name)
    (with-open-file (s name :direction :output :if-does-not-exist :create)
      (declare (ignore s)))))

;;; Create fixture files in sandbox for CLTEST: logical pathname tests
(dolist (name '("truename.txt" "probe-file.txt" "file-author.txt"
                "file-write-date.txt" "ensure-directories-exist.txt" "file-error.txt"))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (unless (probe-file sandbox-name)
      (with-open-file (s sandbox-name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; Create additional fixture files in sandbox to satisfy DIRECTORY.8 (needs >= 30 files)
(dotimes (i 20)
  (let ((name (format nil "sandbox/fixture-~2,'0D.txt" i)))
    (unless (probe-file name)
      (with-open-file (s name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; Clean up scratch directory from previous test runs (needed by ensure-directories-exist.8)
(ignore-errors (dotcl-delete-directory "scratch/"))
(ignore-errors (delete-file "scratch/foo.txt"))

;;; Load files tests
(format t "=== Loading files tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/files/load.lsp")
  (cl-user::%ansi-cat-end "files" %s))

;;; Copy system-construction test fixture files from ansi-test/sandbox/ to sandbox/ and project root
(dolist (name '("compile-file-test-file.lsp" "compile-file-test-file-2.lsp"
                "compile-file-test-file-2a.lsp" "compile-file-test-file-3.lsp"
                "compile-file-test-file-4.lsp" "compile-file-test-file-5.lsp"
                "load-test-file.lsp" "load-test-file-2.lsp"
                "modules7.lsp" "modules8a.lsp" "modules8b.lsp"))
  (let ((src (concatenate 'string "ansi-test/sandbox/" name)))
    (when (probe-file src)
      ;; Copy to project root (for compile-file and modules tests)
      (unless (probe-file name)
        (with-open-file (in src :direction :input)
          (with-open-file (out name :direction :output :if-does-not-exist :create :if-exists :supersede)
            (loop for line = (read-line in nil nil)
                  while line do (write-line line out)))))
      ;; Copy to sandbox/ (for load tests)
      (let ((sandbox-name (concatenate 'string "sandbox/" name)))
        (unless (probe-file sandbox-name)
          (with-open-file (in src :direction :input)
            (with-open-file (out sandbox-name :direction :output :if-does-not-exist :create :if-exists :supersede)
              (loop for line = (read-line in nil nil)
                    while line do (write-line line out)))))))))

;;; Load system-construction tests
(format t "=== Loading system-construction tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/system-construction/load.lsp")
  (cl-user::%ansi-cat-end "system-construction" %s))

;;; Load iteration tests
(format t "=== Loading iteration tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/iteration/load.lsp")
  (cl-user::%ansi-cat-end "iteration" %s))

;;; Load eval-and-compile tests
(format t "=== Loading eval-and-compile tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/eval-and-compile/load.lsp")
  (cl-user::%ansi-cat-end "eval-and-compile" %s))

;;; Load conditions tests
(format t "=== Loading conditions tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/conditions/load.lsp")
  (cl-user::%ansi-cat-end "conditions" %s))

;;; Load printer tests
(format t "=== Loading printer-aux ===~%")
(compile-and-load "ANSI-TESTS:AUX;printer-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;backquote-aux.lsp")
(format t "=== Loading printer tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/printer/load.lsp")
  (cl-user::%ansi-cat-end "printer" %s))

;;; Load environment tests
(format t "=== Loading environment tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/environment/load.lsp")
  (cl-user::%ansi-cat-end "environment" %s))

;;; Load structures tests
(format t "=== Loading structures tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (let ((*default-pathname-defaults*
         (make-pathname
          :directory (pathname-directory (pathname "ansi-test/structures/load.lsp")))))
    (load "structure-00.lsp")
    (load "structures-01.lsp")
    (load "structures-02.lsp")
    (load "structures-03.lsp")
    (load "structures-04.lsp"))
  (cl-user::%ansi-cat-end "structures" %s))

;;; Load packages tests (before objects to avoid interaction crash — T12)
(format t "=== Loading packages tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/packages/load.lsp")
  (cl-user::%ansi-cat-end "packages" %s))

;;; Load objects tests (handler-case protects against macroexpand-time
;;; errors from unsupported features like define-method-combination long form)
(format t "=== Loading objects tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (cl:handler-case (load "ansi-test/objects/load.lsp")
    (error (c) (format t "~&WARNING: objects/load.lsp partially loaded: ~A~%" c)))
  (cl-user::%ansi-cat-end "objects" %s))

;;; Load reader tests
(format t "=== Loading reader tests ===~%")
(compile-and-load "ANSI-TESTS:AUX;reader-aux.lsp")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/reader/load.lsp")
  (cl-user::%ansi-cat-end "reader" %s))

;;; Load numbers tests
(format t "=== Loading numbers tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/numbers/load.lsp")
  (cl-user::%ansi-cat-end "numbers" %s))

;;; Create fixture files in sandbox/ needed by types-and-classes tests
;;; (FILE-STREAM-CPL opens "class-precedence-lists.txt" with :direction :probe;
;;;  during do-tests, *default-pathname-defaults* = sandbox/, so the file
;;;  must exist there)
(unless (probe-file "sandbox/class-precedence-lists.txt")
  (with-open-file (s "sandbox/class-precedence-lists.txt" :direction :output :if-does-not-exist :create)
    (declare (ignore s))))

;;; Create scratch directories for streams tests (scratch/foo.txt is used by several tests)
(ensure-directories-exist "scratch/foo.txt")
(ensure-directories-exist "sandbox/scratch/foo.txt")

;;; Create test data files expected by streams tests (in cwd AND sandbox/)
(dolist (name '("file-position.txt" "file-length.txt" "input-stream-p.txt"
                "output-stream-p.txt" "open-stream-p.txt" "listen.txt"))
  (with-open-file (s name :direction :output :if-exists :supersede)
    (write-string "; test data file" s)
    (terpri s))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (with-open-file (s sandbox-name :direction :output :if-exists :supersede)
      (write-string "; test data file" s)
      (terpri s))))

;;; Load streams tests
(format t "=== Loading streams tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/streams/load.lsp")
  (cl-user::%ansi-cat-end "streams" %s))

;;; Load misc tests
(format t "=== Loading misc tests ===~%")
(let ((%s (cl-user::%ansi-cat-start)))
  (load "ansi-test/misc/load.lsp")
  (cl-user::%ansi-cat-end "misc" %s))

;;; Patch do-entries: crash protection + *hang-tests* filter
;;; Original block/handler-bind bug (D093) has been fixed; return-from restored.
(in-package :regression-test)
(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (the list (cdr *entries*)) :key #'pend)
          (length (cdr *entries*)))
  (finish-output s)
  (dolist (entry (cdr *entries*))
    (when (and (pend entry)
               (not (has-disabled-note entry))
               (not (member (name entry) *hang-tests*)))
      (let ((success? (handler-case (do-entry entry s)
                        (error (c)
                          (format s "~&Test ~:@(~S~) CRASHED: ~A~%" (name entry) c)
                          (push (name entry) *failed-tests*)
                          nil))))
        (format s "~@[~<~%~:; ~:@(~S~)~>~]" success?)
        (finish-output s)
        (if success?
            (push (name entry) *passed-tests*)
            (progn
              (unless (member (name entry) *failed-tests*)
                (push (name entry) *failed-tests*))
              (when (and (boundp '*stop-on-failure*) *stop-on-failure*)
                (finish-output s)
                (return-from do-entries nil)))))))
  (let ((pending (pending-tests))
        (expected-table (make-hash-table :test #'equal)))
    (dolist (ex *expected-failures*)
      (setf (gethash ex expected-table) t))
    (let ((new-failures
           (loop for pend in pending
                 unless (gethash pend expected-table)
                 collect pend)))
      (if (null pending)
          (format s "~&No tests failed.")
        (progn
          (format t "~&~A out of ~A total tests failed: ~%(~{~a~^~%~})"
                  (length pending)
                  (length (cdr *entries*))
                  pending)
          (if (null new-failures)
              (format s "~&No unexpected failures.")
            (when *expected-failures*
              (setf *unexpected-failures* new-failures)
              (format s "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                    (length new-failures)
                    new-failures)))
          (when *expected-failures*
            (let ((pending-table (make-hash-table :test #'equal)))
              (dolist (ex pending)
                (setf (gethash ex pending-table) t))
              (let ((unexpected-successes
                     (loop :for ex :in *expected-failures*
                       :unless (gethash ex pending-table) :collect ex)))
                (if unexpected-successes
                    (progn
                      (setf *unexpected-successes* unexpected-successes)
                      (format t "~&~:D unexpected successes: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                              (length unexpected-successes)
                              unexpected-successes))
                    (format t "~&No unexpected successes.")))))
          ))
      (finish-output s)
      (null pending))))
(in-package :cl-user)

;;; Tests that hang or crash the test runner
(in-package :regression-test)
(setq *hang-tests* '())
(in-package :cl-user)

;;; Run the tests — must be in CL-TEST package since tests use read-from-string
;;; and expect symbols to be interned in CL-TEST (where they were defined)
(in-package :cl-test)
(format t "~%=== Running tests ===~%")
;; Unbind *load-pathname*/*load-truename* so tests see NIL (as if running from REPL)
(let ((*load-pathname* nil) (*load-truename* nil))
  (rt:do-tests))

(in-package :cl-user)
;;; === Per-category summary (for ansi-state.json auto-update) ===
;;; Map test names to categories by checking which load.lsp file defined them.
;;; We use a simple heuristic: match test name prefixes to category directories.
(setf *ansi-category-ranges* (nreverse *ansi-category-ranges*))
(let ((failed-ht (make-hash-table :test #'equal))
      (categories *ansi-category-ranges*))
  ;; Build hash table of failed tests
  (dolist (name (regression-test::pending-tests))
    (setf (gethash name failed-ht) t))
  (format t "~%~%=== Per-category results ===~%")
  (format t "{~%")
  (let ((first t))
    (dolist (cat categories)
      (let* ((cat-name (first cat))
             (start-idx (second cat))
             (end-idx (third cat))
             (entries (cdr regression-test::*entries*))
             (total 0)
             (pass 0))
        (loop for entry in entries
              for idx from 0
              when (and (>= idx start-idx) (< idx end-idx))
              do (incf total)
                 (unless (gethash (regression-test::name entry) failed-ht)
                   (incf pass)))
        (if first (setf first nil) (format t ",~%"))
        (format t "  ~S: {\"tests\": ~D, \"pass\": ~D}" cat-name total pass))))
  (format t "~%}~%"))
