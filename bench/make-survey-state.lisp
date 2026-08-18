;;;; make-survey-state.lisp — merge dotcl + sbcl survey stderr into bench-state.json
;;;;
;;;; Usage: dotcl bench/make-survey-state.lisp <dotcl-stderr> <sbcl-stderr> [existing-bench-state.json]
;;;;
;;;; Each stderr file holds a JSON-ish block written by bench/run.lisp:
;;;;
;;;;   scalar mode (*bench-runs* = NIL)   "tak": 0.516
;;;;   survey mode (*bench-runs* = N)     "tak": [0.484, 0.485, 0.515]
;;;;   error / timeout                    "tak": null
;;;;
;;;; This reads both files, computes median / min / max / stddev / cv for the
;;;; array form, and writes the merged state to stdout. Scalar `dotcl` / `sbcl`
;;;; hold the median so a consumer can keep reading them as plain numbers, and
;;;; entries of the existing state that this run did not produce are carried over.
;;;;
;;;; It replaces make-survey-state.py: the benchmark harness is public, and a
;;;; Lisp implementation aggregating a Lisp benchmark keeps python3 out of that
;;;; path. Output is intended to stay byte-for-byte identical to the Python
;;;; version, so the two can be diffed against the same input while both exist.

(defpackage :bench-survey-state
  (:use :cl)
  (:export #:main))
(in-package :bench-survey-state)

;;; ------------------------------------------------------------------
;;; Values
;;;
;;; A parsed value is one of
;;;   :null            null
;;;   a double-float   scalar
;;;   an integer       an integer straight out of an existing state file
;;;   a string         a status string out of an existing state file
;;;   (:array . list)  the sample list of a survey run
;;;   (:object . alist) an object out of an existing state file (a stats block)
;;; ------------------------------------------------------------------

(defun array-value-p (v) (and (consp v) (eq (car v) :array)))
(defun object-value-p (v) (and (consp v) (eq (car v) :object)))

;;; ------------------------------------------------------------------
;;; Number formatting
;;;
;;; Rounding goes through RATIONAL so the decimal digit is decided on the exact
;;; binary value of the double, ties to even — what Python's round(x, n) does.
;;; Rounding the scaled double instead would decide some boundary cases on the
;;; error introduced by the scaling.
;;; ------------------------------------------------------------------

(defun round-to (x digits)
  (let ((scale (expt 10 digits)))
    (float (/ (round (* (rational x) scale)) scale) 1.0d0)))

(defun format-number (v)
  ;; Integers print bare; doubles print in the shortest form that reads back,
  ;; which is what repr() gives on the Python side.
  (if (integerp v)
      (format nil "~d" v)
      (let ((*read-default-float-format* 'double-float))
        (format nil "~a" v))))

;;; ------------------------------------------------------------------
;;; Reading the stderr blocks
;;;
;;; The Python version scans the whole text with
;;;   "([^"]+)":\s*(\[[^\]]*\]|null|-?[0-9.]+)
;;; and lets a later occurrence of a name overwrite an earlier one while keeping
;;; the position of the first. The scan below does the same by hand rather than
;;; pulling in a regex contrib.
;;; ------------------------------------------------------------------

(defconstant +vertical-tab+ (code-char 11))

(defun whitespace-p (c)
  ;; The set Python's \s matches, which is what the reference regex used.
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)
      (char= c #\Return) (char= c #\Page) (char= c +vertical-tab+)))

(defun number-char-p (c)
  (or (digit-char-p c) (char= c #\.)))

(defun parse-number (string start end)
  "Parse STRING[START:END) as a JSON number. Returns (values value next-index)
   or NIL when it does not start with one. Builds the value as a rational and
   coerces once, so the result is the correctly-rounded double."
  (let ((i start) (negative nil))
    (when (and (< i end) (char= (char string i) #\-))
      (setf negative t) (incf i))
    (let ((int-start i) (int 0))
      (loop while (and (< i end) (digit-char-p (char string i)))
            do (setf int (+ (* int 10) (digit-char-p (char string i)))) (incf i))
      (let ((had-int (> i int-start))
            (fraction 0) (fraction-digits 0) (had-dot nil))
        (when (and (< i end) (char= (char string i) #\.))
          (setf had-dot t) (incf i)
          (loop while (and (< i end) (digit-char-p (char string i)))
                do (setf fraction (+ (* fraction 10) (digit-char-p (char string i))))
                   (incf fraction-digits)
                   (incf i)))
        (unless (or had-int (> fraction-digits 0))
          (return-from parse-number nil))
        (let* ((magnitude (+ int (if (zerop fraction-digits)
                                     0
                                     (/ fraction (expt 10 fraction-digits)))))
               (value (if negative (- magnitude) magnitude)))
          (values (if (and (not had-dot) (integerp value))
                      value
                      (float value 1.0d0))
                  i))))))

(defun parse-array (string start end)
  "Parse STRING[START:END) as [n, n, ...] up to the first ]. Returns
   (values (:array . samples) next-index) or NIL."
  (let ((close (position #\] string :start start :end end)))
    (unless close (return-from parse-array nil))
    (let ((samples '())
          (i (1+ start)))
      (loop
        (loop while (and (< i close)
                         (or (whitespace-p (char string i)) (char= (char string i) #\,)))
              do (incf i))
        (when (>= i close) (return))
        (multiple-value-bind (value next) (parse-number string i close)
          (unless next (return))
          (push (float value 1.0d0) samples)
          (setf i next)))
      (values (cons :array (nreverse samples)) (1+ close)))))

(defun parse-value (string start end)
  "Parse the value at STRING[START:END). Returns (values value next-index) or NIL
   when what is there is not one of the three accepted shapes."
  (when (>= start end) (return-from parse-value nil))
  (let ((c (char string start)))
    (cond
      ((char= c #\[) (parse-array string start end))
      ((and (<= (+ start 4) end) (string= "null" string :start2 start :end2 (+ start 4)))
       (values :null (+ start 4)))
      ((or (char= c #\-) (number-char-p c)) (parse-number string start end))
      (t nil))))

(defun parse-results (text)
  "Every \"name\": value pair in TEXT, in order of first appearance. Returns a
   list of (name . value); a repeated name updates its value in place."
  (let ((entries '())              ; reversed list of (name . value) conses
        (end (length text))
        (i 0))
    (loop while (< i end) do
      (let ((quote-start (position #\" text :start i)))
        (unless quote-start (return))
        (let ((quote-end (position #\" text :start (1+ quote-start))))
          (unless quote-end (return))
          (let ((name (subseq text (1+ quote-start) quote-end))
                (after (1+ quote-end)))
            (cond
              ((and (plusp (length name))
                    (< after end)
                    (char= (char text after) #\:))
               (let ((value-start (1+ after)))
                 (loop while (and (< value-start end) (whitespace-p (char text value-start)))
                       do (incf value-start))
                 (multiple-value-bind (value next) (parse-value text value-start end)
                   (cond (next
                          (let ((hit (assoc name entries :test #'string=)))
                            (if hit
                                (setf (cdr hit) value)
                                (push (cons name value) entries)))
                          (setf i next))
                         (t (setf i (1+ quote-start)))))))
              (t (setf i (1+ quote-start))))))))
    (nreverse entries)))

;;; ------------------------------------------------------------------
;;; Reading the existing state file
;;;
;;; Only the subset this tool writes: an object of objects whose values are
;;; numbers, strings, null, or a nested stats object. Anything unexpected makes
;;; the whole file be ignored, as a JSON decode error does on the Python side.
;;; ------------------------------------------------------------------

(define-condition json-error (error) ())

(defun json-skip-space (s i end)
  (loop while (and (< i end) (whitespace-p (char s i))) do (incf i))
  i)

(defun json-parse-string (s i end)
  (unless (and (< i end) (char= (char s i) #\")) (error 'json-error))
  (let ((close (position #\" s :start (1+ i) :end end)))
    (unless close (error 'json-error))
    (values (subseq s (1+ i) close) (1+ close))))

(defun json-parse-value (s i end)
  (setf i (json-skip-space s i end))
  (when (>= i end) (error 'json-error))
  (let ((c (char s i)))
    (cond
      ((char= c #\{) (json-parse-object s i end))
      ((char= c #\") (json-parse-string s i end))
      ((and (<= (+ i 4) end) (string= "null" s :start2 i :end2 (+ i 4)))
       (values :null (+ i 4)))
      ((or (char= c #\-) (number-char-p c))
       (multiple-value-bind (value next) (parse-number s i end)
         (unless next (error 'json-error))
         (values value next)))
      (t (error 'json-error)))))

(defun json-parse-object (s i end)
  (unless (and (< i end) (char= (char s i) #\{)) (error 'json-error))
  (incf i)
  (let ((pairs '()))
    (setf i (json-skip-space s i end))
    (when (and (< i end) (char= (char s i) #\}))
      (return-from json-parse-object (values (cons :object nil) (1+ i))))
    (loop
      (setf i (json-skip-space s i end))
      (multiple-value-bind (key next) (json-parse-string s i end)
        (setf i (json-skip-space s next end))
        (unless (and (< i end) (char= (char s i) #\:)) (error 'json-error))
        (multiple-value-bind (value vnext) (json-parse-value s (1+ i) end)
          (push (cons key value) pairs)
          (setf i (json-skip-space s vnext end))))
      (cond ((and (< i end) (char= (char s i) #\,)) (incf i))
            ((and (< i end) (char= (char s i) #\})) (incf i) (return))
            (t (error 'json-error))))
    (values (cons :object (nreverse pairs)) i)))

(defun read-file-string (path)
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (unless in (return-from read-file-string nil))
    (let ((buffer (make-string (file-length in))))
      (let ((n (read-sequence buffer in)))
        (subseq buffer 0 n)))))

(defun existing-benchmarks (path)
  "The benchmarks object of an existing state file, as a list of (name . value).
   NIL when the file is absent or does not parse."
  (let ((text (and path (read-file-string path))))
    (unless text (return-from existing-benchmarks nil))
    (handler-case
        (multiple-value-bind (value next) (json-parse-value text 0 (length text))
          (declare (ignore next))
          (unless (object-value-p value) (return-from existing-benchmarks nil))
          (let ((benchmarks (cdr (assoc "benchmarks" (cdr value) :test #'string=))))
            (if (object-value-p benchmarks) (cdr benchmarks) nil)))
      (error () nil))))

;;; ------------------------------------------------------------------
;;; Statistics
;;; ------------------------------------------------------------------

(defun median-of (sorted)
  (let ((n (length sorted)))
    (if (oddp n)
        (nth (floor n 2) sorted)
        (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2))))

(defun sample-stddev (samples)
  "Sample standard deviation (N-1). The sum of squares is exact, as Python's
   statistics.stdev computes it, so only the final square root rounds."
  (let* ((n (length samples))
         (mean (/ (reduce #'+ (mapcar #'rational samples)) n))
         (ss (reduce #'+ (mapcar (lambda (x) (expt (- (rational x) mean) 2)) samples))))
    (sqrt (float (/ ss (1- n)) 1.0d0))))

(defun stats-of (samples)
  "The stats block for SAMPLES as an (:object . alist), keys in write order."
  (let* ((sorted (sort (copy-list samples) #'<))
         (median (median-of sorted))
         (stddev (if (>= (length samples) 2) (sample-stddev samples) 0.0d0))
         (cv (if (> median 0) (/ stddev median) 0.0d0)))
    (cons :object
          (list (cons "median" (round-to median 3))
                (cons "min" (round-to (first sorted) 3))
                (cons "max" (round-to (car (last sorted)) 3))
                (cons "stddev" (round-to stddev 3))
                (cons "cv" (round-to cv 3))
                (cons "runs" (length samples))))))

;;; ------------------------------------------------------------------
;;; Entries
;;; ------------------------------------------------------------------

(defun entry-get (entry key)
  (cdr (assoc key entry :test #'string=)))

(defun entry-put (entry key value)
  "ENTRY with KEY appended (write order is fixed later by FORMAT-ENTRY)."
  (append entry (list (cons key value))))

(defun build-entry (dotcl-value sbcl-value)
  (let ((entry '()))
    (cond ((eq dotcl-value :null)
           (setf entry (entry-put entry "dotcl" :null))
           (setf entry (entry-put entry "status" "dotcl-error")))
          ((array-value-p dotcl-value)
           (let ((stats (stats-of (cdr dotcl-value))))
             (setf entry (entry-put entry "dotcl" (entry-get (cdr stats) "median")))
             (setf entry (entry-put entry "dotcl_stats" stats))))
          (t (setf entry (entry-put entry "dotcl" (round-to dotcl-value 3)))))

    (cond ((eq sbcl-value :null)
           (setf entry (entry-put entry "sbcl" :null))
           (unless (entry-get entry "status")
             (setf entry (entry-put entry "status" "sbcl-error"))))
          ((array-value-p sbcl-value)
           (let ((stats (stats-of (cdr sbcl-value))))
             (setf entry (entry-put entry "sbcl" (entry-get (cdr stats) "median")))
             (setf entry (entry-put entry "sbcl_stats" stats))))
          (t (setf entry (entry-put entry "sbcl" (round-to sbcl-value 3)))))

    (let ((d (entry-get entry "dotcl"))
          (b (entry-get entry "sbcl")))
      (cond ((and (not (eq d :null)) (not (eq b :null)))
             (cond ((> b 0) (setf entry (entry-put entry "ratio" (round-to (/ d b) 1))))
                   (t (setf entry (entry-put entry "ratio" :null))
                      (setf entry (entry-put entry "status" "sbcl-zero")))))
            (t (setf entry (entry-put entry "ratio" :null)))))
    entry))

;;; ------------------------------------------------------------------
;;; Writing
;;; ------------------------------------------------------------------

(defun format-value (value)
  (cond ((eq value :null) "null")
        ((stringp value) (format nil "\"~a\"" value))
        ((object-value-p value)
         (format nil "{~{~a~^, ~}}"
                 (mapcar (lambda (pair)
                           (format nil "\"~a\": ~a" (car pair) (format-value (cdr pair))))
                         (cdr value))))
        (t (format-number value))))

(defun format-entry (entry)
  "One entry as a single-line object, in the fixed key order."
  (let ((parts '()))
    (dolist (key '("dotcl" "sbcl" "ratio" "dotcl_stats" "sbcl_stats" "status"))
      (let ((pair (assoc key entry :test #'string=)))
        (when pair
          (push (format nil "\"~a\": ~a" key (format-value (cdr pair))) parts))))
    (format nil "{~{~a~^, ~}}" (nreverse parts))))

(defun today-string ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

;;; ------------------------------------------------------------------
;;; Main
;;; ------------------------------------------------------------------

(defun ends-with-p (suffix string)
  (let ((s (length suffix)) (n (length string)))
    (and (>= n s) (string= suffix string :start2 (- n s)))))

(defun script-arguments ()
  "The file arguments, under either way of starting this script.
   `dotcl make-survey-state.lisp a b c` puts them after a \"--\" marker, while
   the development invocation `dotcl --asm cil-out.sil make-survey-state.lisp a b c`
   loads each remaining path in turn and leaves the host argv untouched — there
   the arguments are whatever follows this file's own path."
  (let* ((args (dotcl:command-line-arguments))
         (delimited (member "--" args :test #'string=)))
    (if delimited
        (rest delimited)
        (let ((self (member-if (lambda (a) (ends-with-p "make-survey-state.lisp" a)) args)))
          (rest self)))))

(defun main ()
  (let ((args (script-arguments)))
    (when (< (length args) 2)
      (format *error-output*
              "Usage: make-survey-state.lisp <dotcl-stderr> <sbcl-stderr> [existing-bench-state.json]~%")
      (return-from main 1))
    (let* ((dotcl-results (parse-results (or (read-file-string (first args)) "")))
           (sbcl-results (parse-results (or (read-file-string (second args)) "")))
           (existing (existing-benchmarks (third args)))
           (new-names (mapcar #'car dotcl-results))
           (all-names (append new-names
                              (remove-if (lambda (name) (member name new-names :test #'string=))
                                         (mapcar #'car existing))))
           (max-runs 1))
      (dolist (pair (append dotcl-results sbcl-results))
        (let ((value (cdr pair)))
          (when (and (array-value-p value) (> (length (cdr value)) max-runs))
            (setf max-runs (length (cdr value))))))
      (let ((entries
              (mapcar (lambda (name)
                        (cons name
                              (if (member name new-names :test #'string=)
                                  (format-entry
                                   (build-entry
                                    (or (cdr (assoc name dotcl-results :test #'string=)) :null)
                                    (or (cdr (assoc name sbcl-results :test #'string=)) :null)))
                                  (format-entry
                                   (cdr (cdr (assoc name existing :test #'string=)))))))
                      all-names)))
        (format t "{~%")
        (format t "  \"updated\": \"~a\",~%" (today-string))
        (when (> max-runs 1)
          (format t "  \"survey_config\": {\"runs\": ~d, \"warmup\": 1},~%" max-runs))
        (format t "  \"benchmarks\": {~%")
        (loop for rest on entries
              for pair = (car rest)
              do (format t "    \"~a\": ~a~a~%"
                         (car pair) (cdr pair) (if (cdr rest) "," "")))
        (format t "  }~%")
        (format t "}~%"))
      0)))

;; Exit rather than return: the development invocation passes the input paths as
;; further positional arguments, which dotcl would otherwise go on to load as
;; Lisp source once this file is done.
(finish-output)
(dotcl:quit (main))
