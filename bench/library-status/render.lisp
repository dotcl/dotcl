;;;; render.lisp -- turn the stage 2 results into docs/library-status.md.
;;;;
;;;; Stage 3 of the library-status pipeline (see README.md). Reads the JSON a
;;;; check run produces and writes the table, in the order stage 1 decided:
;;;;
;;;;   [{"system": "alexandria",
;;;;     "release": "alexandria-20241012-git",
;;;;     "status": "ok",              ; ok | load-only | patched | fail
;;;;     "issue": 53,                 ; public dotcl/dotcl issue, or null
;;;;     "checked": "2026-09-14",
;;;;     "note": "..."},              ; optional, may be "" or absent
;;;;    ...]
;;;;
;;;; "note" is a machine field: a check run rewrites it from whatever that run
;;;; saw. So the reason BEHIND a row -- which a person worked out once and which
;;;; no measurement can rediscover -- is kept out of the results, in
;;;;
;;;;   {"swank": "The Quicklisp dist pins SLIME v2.32, whose ...", ...}
;;;;
;;;; and merged here as the "why" column. The check run never opens that file,
;;;; so re-measuring cannot erase it.
;;;;
;;;; Run it with any conforming Lisp:
;;;;
;;;;   sbcl --non-interactive --load bench/library-status/render.lisp
;;;;
;;;; Inputs and outputs are taken from the environment so that one make target
;;;; can drive it (all five have defaults relative to this file):
;;;;
;;;;   LIBRARY_STATUS_JSON         results to render  (default results.json)
;;;;   LIBRARY_STATUS_TARGETS      row order          (default targets.txt)
;;;;   LIBRARY_STATUS_ANNOTATIONS  the why column     (default annotations.json)
;;;;   LIBRARY_STATUS_OUT          markdown to write  (default ../../docs/library-status.md)
;;;;   DOTCL_VERSION               version that was measured (default "unknown")
;;;;
;;;; MAIN takes the same five as keyword arguments, for calling it by hand.
;;;;
;;;; The table is published, so the "issue" field must name an issue in the
;;;; public dotcl/dotcl repository, and an annotation sentence is published
;;;; verbatim. A target tracked somewhere a reader cannot open leaves the field
;;;; null and says what is wrong in "note" instead.

(defpackage :library-status/render
  (:use :cl)
  (:export #:main))

(in-package :library-status/render)

(defun %getenv (name)
  (declare (ignorable name))
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+clisp (ext:getenv name)
  #+ecl (ext:getenv name)
  #+dotcl (dotnet:static "System.Environment" "GetEnvironmentVariable" name)
  #-(or sbcl ccl clisp ecl dotcl) nil)

(defun %env (name default)
  (let ((value (%getenv name)))
    (if (and value (plusp (length value))) value default)))

;;; --- a small JSON reader ---------------------------------------------------
;;;
;;; Only what this file consumes: an array of objects whose values are strings,
;;; integers, booleans or null. Bringing a JSON library in would mean the table
;;; could not be regenerated without first installing one, which is the opposite
;;; of what a status table is for.

(define-condition json-error (simple-error) ())

(defun %json-error (control &rest args)
  (error 'json-error :format-control control :format-arguments args))

(defstruct (cursor (:conc-name cur-))
  (text "" :type string)
  (pos 0 :type fixnum))

(defun %peek (c)
  (when (< (cur-pos c) (length (cur-text c)))
    (char (cur-text c) (cur-pos c))))

(defun %next (c)
  (let ((ch (%peek c)))
    (unless ch (%json-error "Unexpected end of JSON input"))
    (incf (cur-pos c))
    ch))

(defun %skip-space (c)
  (loop for ch = (%peek c)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=))
        do (incf (cur-pos c))))

(defun %expect (c expected)
  (%skip-space c)
  (let ((ch (%next c)))
    (unless (char= ch expected)
      (%json-error "Expected ~C at position ~D, got ~C" expected (1- (cur-pos c)) ch))))

(defun %read-json-string (c)
  (%expect c #\")
  (with-output-to-string (out)
    (loop for ch = (%next c)
          until (char= ch #\")
          do (if (char= ch #\\)
                 (let ((esc (%next c)))
                   (case esc
                     (#\n (write-char #\Newline out))
                     (#\t (write-char #\Tab out))
                     (#\r (write-char #\Return out))
                     (#\b (write-char #\Backspace out))
                     (#\f (write-char #\Page out))
                     (#\u (let ((code 0))
                            (dotimes (i 4)
                              (setf code (+ (* 16 code)
                                            (digit-char-p (%next c) 16))))
                            (write-char (code-char code) out)))
                     (t (write-char esc out))))
                 (write-char ch out)))))

(defun %read-json-number (c)
  (let ((start (cur-pos c)))
    (loop for ch = (%peek c)
          while (and ch (or (digit-char-p ch)
                            (member ch '(#\- #\+ #\. #\e #\E) :test #'char=)))
          do (incf (cur-pos c)))
    (let ((text (subseq (cur-text c) start (cur-pos c))))
      (if (find-if (lambda (ch) (member ch '(#\. #\e #\E) :test #'char=)) text)
          ;; No float syntax is expected here; keep it as text rather than
          ;; guessing a reader setting.
          text
          (parse-integer text)))))

(defun %read-json-literal (c)
  (let ((start (cur-pos c)))
    (loop for ch = (%peek c)
          while (and ch (alpha-char-p ch))
          do (incf (cur-pos c)))
    (let ((word (subseq (cur-text c) start (cur-pos c))))
      (cond ((string= word "true") t)
            ((string= word "false") nil)
            ((string= word "null") nil)
            (t (%json-error "Unknown JSON literal ~S" word))))))

;; Values, arrays and objects are mutually recursive; say so before the first
;; one, or a file compiler has to warn about the two it has not seen yet.
(declaim (ftype (function (t) t) %read-json-array %read-json-object))

(defun %read-json-value (c)
  (%skip-space c)
  (let ((ch (%peek c)))
    (case ch
      (#\{ (%read-json-object c))
      (#\[ (%read-json-array c))
      (#\" (%read-json-string c))
      ((nil) (%json-error "Unexpected end of JSON input"))
      (t (if (or (digit-char-p ch) (char= ch #\-))
             (%read-json-number c)
             (%read-json-literal c))))))

(defun %read-json-array (c)
  (%expect c #\[)
  (%skip-space c)
  (if (eql (%peek c) #\])
      (progn (%next c) '())
      (let ((items '()))
        (loop
          (push (%read-json-value c) items)
          (%skip-space c)
          (let ((ch (%next c)))
            (case ch
              (#\, nil)
              (#\] (return (nreverse items)))
              (t (%json-error "Expected , or ] in array, got ~C" ch))))))))

(defun %read-json-object (c)
  "An object as an alist of (KEY . VALUE), keys as strings."
  (%expect c #\{)
  (%skip-space c)
  (if (eql (%peek c) #\})
      (progn (%next c) '())
      (let ((pairs '()))
        (loop
          (%skip-space c)
          (let ((key (%read-json-string c)))
            (%expect c #\:)
            (push (cons key (%read-json-value c)) pairs))
          (%skip-space c)
          (let ((ch (%next c)))
            (case ch
              (#\, nil)
              (#\} (return (nreverse pairs)))
              (t (%json-error "Expected , or } in object, got ~C" ch))))))))

(defun %parse-json (text)
  (%read-json-value (make-cursor :text text :pos 0)))

(defun %read-file (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun %field (object key)
  (cdr (assoc key object :test #'string=)))

;;; --- the row order ---------------------------------------------------------

(defparameter *hand-picked-marker* "# --- hand-picked (not ranked) ---"
  "Written by rank.lisp to separate the two halves of targets.txt. Everything
after it was chosen by hand, so it is rendered as its own table -- putting rows
ordered by referrer count and rows ordered by nothing in one list would claim a
ranking for the second half that does not exist.")

(defun %read-targets (path)
  "(values RANKED HAND-PICKED DIST-DESCRIPTION) from a stage 1 targets file."
  (let ((systems '())
        (hand-picked '())
        (in-hand-picked nil)
        (dist "unknown"))
    (with-open-file (in path :direction :input :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                 (cond ((zerop (length trimmed)))
                       ((string= trimmed *hand-picked-marker*)
                        (setf in-hand-picked t))
                       ((char= (char trimmed 0) #\#)
                        (let ((tag "# dist:"))
                          (when (and (>= (length trimmed) (length tag))
                                     (string= tag trimmed :end2 (length tag)))
                            (setf dist (string-trim " " (subseq trimmed (length tag)))))))
                       (in-hand-picked (push trimmed hand-picked))
                       (t (push trimmed systems))))))
    (values (nreverse systems) (nreverse hand-picked) dist)))

;;; --- markdown --------------------------------------------------------------

(defparameter *status-legend*
  '(("ok" . "loads, and the library's own test suite passes")
    ("load-only" . "loads; its test suite has not been run here")
    ("patched" . "loads from the patched release in the dotcl dist overlay")
    ("fail" . "does not load")
    ("not checked" . "not measured in this run"))
  "The status values a row may carry, in the order they are explained.")

(defun %escape-cell (text)
  "Markdown table cells are separated by |, so a | inside one has to be escaped."
  (with-output-to-string (out)
    (loop for ch across text
          do (when (char= ch #\|) (write-char #\\ out))
             (write-char (if (char= ch #\Newline) #\Space ch) out))))

(defun %longest-backtick-run (text)
  (let ((best 0) (run 0))
    (loop for ch across text
          do (if (char= ch #\`)
                 (progn (incf run) (when (> run best) (setf best run)))
                 (setf run 0)))
    best))

(defun %code-cell (text)
  "TEXT as a Markdown code span, or \"\" when there is nothing to show.

A note is a raw error message, and Markdown reads one: `Unbound variable:
*SYSDEP-FILES*` renders as \"Unbound variable: SYSDEP-FILES\" in italics, having
eaten the asterisks that were the whole point. Escaping the significant
characters one by one means keeping a list of them, so the cell becomes a code
span instead -- everything inside one is literal, which is also what an error
message IS.

The fence is one backtick longer than the longest run inside the text
(CommonMark 6.1), and content that starts or ends with a backtick is padded with
a space, which the code-span rule strips again. The | escaping stays: a table row
is split on | before any of this is looked at, so it is needed inside a code span
too."
  (if (zerop (length text))
      ""
      (let* ((flat (%escape-cell text))
             (fence (make-string (1+ (%longest-backtick-run flat))
                                 :initial-element #\`))
             (pad (if (or (char= (char flat 0) #\`)
                          (char= (char flat (1- (length flat))) #\`))
                      " "
                      "")))
        (concatenate 'string fence pad flat pad fence))))

(defun %issue-cell (issue)
  (cond ((null issue) "")
        ((integerp issue)
         (format nil "[dotcl/dotcl#~D](https://github.com/dotcl/dotcl/issues/~D)"
                 issue issue))
        (t (%escape-cell (princ-to-string issue)))))

(defun %annotation (annotations system)
  "The hand-written sentence for SYSTEM, or NIL. Keys beginning with // are
comments in the file and never name a system."
  (let ((value (cdr (assoc system annotations :test #'string=))))
    (and (stringp value) (plusp (length value)) value)))

(defun %read-annotations (path)
  "The annotations file as an alist, or NIL when there is none.

A check run rewrites NOTE from whatever the run saw, so a reason written there
by hand is erased by the next measurement. The reasons live here instead, in a
file the run never opens. Absent is not an error: the table is still renderable
from the results alone, it just has an empty WHY column."
  (if (probe-file path)
      (let ((parsed (%parse-json (%read-file path))))
        (unless (listp parsed)
          (%json-error "~A: expected an object of system -> sentence" path))
        ;; JSON has no comment syntax, so the file carries its own instructions
        ;; under // keys. Drop them here, once, rather than teaching every
        ;; reader of the alist to skip them.
        (remove-if (lambda (pair)
                     (let ((key (car pair)))
                       (and (>= (length key) 2) (string= "//" key :end2 2))))
                   parsed))
      '()))

(defun %row-for (system by-system)
  "The result object for SYSTEM, or a placeholder saying it was not measured."
  (or (gethash system by-system)
      (list (cons "system" system)
            (cons "status" "not checked"))))

(defun %write-rows (out rows annotations)
  (format out "~%| system | status | issue | checked | why | note |~%")
  (format out "| --- | --- | --- | --- | --- | --- |~%")
  (dolist (row rows)
    (let ((system (or (%field row "system") "?")))
      (format out "| ~A | `~A` | ~A | ~A | ~A | ~A |~%"
              (%escape-cell system)
              (or (%field row "status") "not checked")
              (%issue-cell (%field row "issue"))
              (%escape-cell (or (%field row "checked") ""))
              ;; Prose a person wrote, so it is rendered as prose; only the cell
              ;; separator has to be dealt with.
              (%escape-cell (or (%annotation annotations system) ""))
              (%code-cell (or (%field row "note") ""))))))

(defun %write-markdown (path rows dist dotcl-version extra annotations
                        &optional hand-picked-rows)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (format out "# Library status~%~%")
    (format out "Which Common Lisp libraries run on dotcl, most depended upon~%")
    (format out "first. Generated by `bench/library-status/render.lisp` -- do not~%")
    (format out "edit this file by hand.~%~%")
    (format out "- Quicklisp dist: ~A~%" dist)
    (format out "- dotcl: ~A~%" dotcl-version)
    (format out "- Rows: ~D~%~%" (+ (length rows) (length extra) (length hand-picked-rows)))
    (format out "Order is how many other Quicklisp projects depend on a project,~%")
    (format out "counted directly rather than transitively. One row per release:~%")
    (format out "a library and its test system are one thing to install and one~%")
    (format out "row here.~%~%")
    (format out "| status | meaning |~%| --- | --- |~%")
    (loop for (status . meaning) in *status-legend*
          do (format out "| `~A` | ~A |~%" status meaning))
    (%write-rows out (append rows extra) annotations)
    (when hand-picked-rows
      (format out "~%## Hand-picked~%~%")
      (format out "Chosen by hand rather than by referrer count: an application,~%")
      (format out "or a library whose value is that it stresses one part of the~%")
      (format out "compiler hard, sits at the end of the dependency graph and~%")
      (format out "ranks nowhere. The order in this table means nothing.~%")
      (%write-rows out hand-picked-rows annotations))))

(defun main (&key json targets out dotcl-version annotations)
  (let* ((here (directory-namestring
                (or *load-truename* *default-pathname-defaults*)))
         (json (or json (%env "LIBRARY_STATUS_JSON"
                              (namestring (merge-pathnames "results.json" here)))))
         (annotations (or annotations
                          (%env "LIBRARY_STATUS_ANNOTATIONS"
                                (namestring (merge-pathnames "annotations.json" here)))))
         (targets (or targets (%env "LIBRARY_STATUS_TARGETS"
                                    (namestring (merge-pathnames "targets.txt" here)))))
         (out (or out (%env "LIBRARY_STATUS_OUT"
                            (namestring (merge-pathnames "../../docs/library-status.md"
                                                         here)))))
         (dotcl-version (or dotcl-version (%env "DOTCL_VERSION" "unknown"))))
    (multiple-value-bind (order hand-picked dist) (%read-targets targets)
      (let ((results (%parse-json (%read-file json)))
            (annotations (%read-annotations annotations))
            (by-system (make-hash-table :test #'equal)))
        (dolist (result results)
          (let ((system (%field result "system")))
            (if system
                (setf (gethash system by-system) result)
                (warn "library-status: a result has no \"system\" field; skipped"))))
        (let* ((rows (mapcar (lambda (system) (%row-for system by-system)) order))
               (hand-picked-rows (mapcar (lambda (system) (%row-for system by-system))
                                         hand-picked))
               ;; A result for something outside the target list is still shown,
               ;; after the ranked rows: it was measured, and dropping it would
               ;; hide work rather than tidy the table.
               (extra (remove-if (lambda (result)
                                   (let ((s (%field result "system")))
                                     (or (member s order :test #'string=)
                                         (member s hand-picked :test #'string=))))
                                 results)))
          (%write-markdown out rows dist dotcl-version extra annotations
                           hand-picked-rows)
          (format t "~&wrote ~A~%" out)
          (format t "~D ranked rows (~D measured), ~D hand-picked, ~D extra, ~D annotated~%"
                  (length rows)
                  (count-if (lambda (row) (gethash (%field row "system") by-system)) rows)
                  (length hand-picked-rows)
                  (length extra)
                  (count-if (lambda (pair) (%annotation annotations (car pair)))
                            annotations))
          (length rows))))))

(main)
