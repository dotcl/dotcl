;;; ANSI test runner - single category
;;; Usage: dotnet run --project runtime -- --asm compiler/cil-out.sil test-ansi-cat.lisp
;;; Category is appended to this file by the Makefile target

(in-package :cl-user)

(defvar *aux-dir* (pathname "ansi-test/auxiliary/"))

(defun compile-and-load (path &key force)
  (declare (ignore force))
  (let* ((s (namestring (pathname path)))
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
  "Load a file relative to *aux-dir* (ansi-test/auxiliary/)."
  (declare (ignore force))
  (load (merge-pathnames path *aux-dir*)))

;;; Load the RT test framework
(load "ansi-test/rt-package.lsp")
(load "ansi-test/rt.lsp")

;;; Load the CL-TEST package
(load "ansi-test/cl-test-package.lsp")

;;; Load common auxiliary files
;;; NOTE: Must stay in CL-TEST package for aux loading, because aux files
;;; (random-aux.lsp, remove-aux.lsp, etc.) have no in-package form.
;;; If loaded in CL-USER, defmacro'd symbols (random-case, rcase) end up
;;; in CL-USER, but category load.lsp files re-load aux files in CL-TEST,
;;; creating different symbols (CL-TEST::RANDOM-CASE vs CL-USER::RANDOM-CASE).
(in-package :cl-test)

;;; Load ansi-aux-macros BEFORE universe.lsp — the gitlab ansi-test repo
;;; shadows handler-case/handler-bind in :cl-test and provides replacements
;;; in ansi-aux-macros.lsp. universe.lsp uses handler-case, so the macros
;;; must be available by the time it loads.
(compile-and-load "ANSI-TESTS:AUX;ansi-aux-macros.lsp")
(load "ansi-test/universe.lsp")
(load "ansi-test/cl-symbol-names.lsp")
(compile-and-load "ANSI-TESTS:AUX;ansi-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;random-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;cons-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;string-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;types-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;printer-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;backquote-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;reader-aux.lsp")

;;; Create fixture files needed by various test categories
(in-package :cl-user)

;;; sandbox directory — must exist before we create any sandbox/* files below.
;;; do-tests in rt.lsp sets *default-pathname-defaults* to (truename #P"sandbox/")
;;; at test-run time, so ALL fixture files must live in sandbox/.
(ensure-directories-exist "sandbox/dummy.txt")

;;; types-and-classes — class-precedence-lists.txt must be in sandbox/
(dolist (name '("class-precedence-lists.txt"))
  (unless (probe-file name)
    (with-open-file (s name :direction :output :if-does-not-exist :create)
      (declare (ignore s))))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (unless (probe-file sandbox-name)
      (with-open-file (s sandbox-name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; pathnames
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

;;; files — create in both project root and sandbox/
(dolist (name '("truename.txt" "probe-file.txt" "file-author.txt"
                "file-write-date.txt" "ensure-directories-exist.txt" "file-error.txt"))
  (unless (probe-file name)
    (with-open-file (s name :direction :output :if-does-not-exist :create)
      (declare (ignore s))))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (unless (probe-file sandbox-name)
      (with-open-file (s sandbox-name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; sandbox needs >= 30 files for DIRECTORY.8
(dotimes (i 20)
  (let ((name (format nil "sandbox/fixture-~2,'0D.txt" i)))
    (unless (probe-file name)
      (with-open-file (s name :direction :output :if-does-not-exist :create)
        (declare (ignore s))))))

;;; Clean up scratch directory (needed by ensure-directories-exist.8)
(ignore-errors (dotcl-delete-directory "scratch/"))
(ignore-errors (delete-file "scratch/foo.txt"))

;;; Re-create scratch directory (needed by streams tests that open scratch/foo.txt)
(ensure-directories-exist "scratch/foo.txt")

;;; streams test data files (in project root AND sandbox/ — do-tests rebinds DPD to sandbox/)
(dolist (name '("file-position.txt" "file-length.txt" "input-stream-p.txt"
                "output-stream-p.txt" "open-stream-p.txt" "listen.txt"))
  (with-open-file (s name :direction :output :if-exists :supersede)
    (write-string "; test data file" s)
    (terpri s))
  (let ((sandbox-name (concatenate 'string "sandbox/" name)))
    (with-open-file (s sandbox-name :direction :output :if-exists :supersede)
      (write-string "; test data file" s)
      (terpri s))))

;;; system-construction test fixture files
(dolist (name '("compile-file-test-file.lsp" "compile-file-test-file-2.lsp"
                "compile-file-test-file-2a.lsp" "compile-file-test-file-3.lsp"
                "compile-file-test-file-4.lsp" "compile-file-test-file-5.lsp"
                "load-test-file.lsp" "load-test-file-2.lsp"
                "modules7.lsp" "modules8a.lsp" "modules8b.lsp"))
  (let ((src (concatenate 'string "ansi-test/sandbox/" name)))
    (when (probe-file src)
      (unless (probe-file name)
        (with-open-file (in src :direction :input)
          (with-open-file (out name :direction :output :if-does-not-exist :create :if-exists :supersede)
            (loop for line = (read-line in nil nil)
                  while line do (write-line line out)))))
      (let ((sandbox-name (concatenate 'string "sandbox/" name)))
        (unless (probe-file sandbox-name)
          (with-open-file (in src :direction :input)
            (with-open-file (out sandbox-name :direction :output :if-does-not-exist :create :if-exists :supersede)
              (loop for line = (read-line in nil nil)
                    while line do (write-line line out)))))))))

;;; Patch do-entries to avoid return-from (workaround for compiler block/handler-bind bug)
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
      (let ((success? (do-entry entry s)))
        (format s "~@[~<~%~:; ~:@(~S~)~>~]" success?)
        (finish-output s)
        (if success?
            (push (name entry) *passed-tests*)
            (push (name entry) *failed-tests*)))))
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
          (finish-output s)
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
(in-package :cl-test)

;;; Tests that hang (infinite loop on circular structures — *print-circle* not yet implemented)
;;; These are skipped in do-entries below via *hang-tests*
(in-package :regression-test)
(defvar *hang-tests* '(cl-test::pprint-fill.15
                       cl-test::pprint-pop.8
                       cl-test::print.cons.7
                       cl-test::print.cons.random.2))
(in-package :cl-test)

;;; Category is loaded below (appended by Makefile)
;;; NOTE: must be in cl-test package here, matching gclload2.lsp convention
