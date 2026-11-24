;;; regression: skip-under-interp
;;; Composed string operations stay within the strings and symbols they return.
;;; Compiled DEFUNs only: the interpreter does not use the direct entries here.

(in-package :cl-user)

(defparameter *sca-iterations* 1000)
(defparameter *sca-runs* 5)
(defparameter *sca-sink* nil)
(defparameter *sca-rows* nil)
(defparameter *sca-baseline* 0)
(defparameter *sca-floor-cache* nil)

(defparameter *sca-pkg* (make-package "STRING-CHAIN-TEST" :use nil))
(defparameter *sca-s* "abcdefghijklmnop")
(defparameter *sca-num* "0012345678901234")
(defparameter *sca-name* "abcdefghijklmnop")
(defparameter *sca-sym* (intern *sca-name* *sca-pkg*))
(defparameter *sca-existing* (intern "CDEFGHIJ" *sca-pkg*))

(defmacro sca-def-row (name args &body body)
  `(defun ,name (,@args n)
     (declare (fixnum n))
     (dotimes (i n)
       (setq *sca-sink* (progn ,@body)))
     *sca-sink*))

(defun sca-add-row (label floor fn &rest args)
  (push (list label floor fn args) *sca-rows*))

(sca-def-row sca-row-subseq (s a b) (subseq s a b))
(sca-add-row "SUBSEQ" '((string 4)) #'sca-row-subseq *sca-s* 3 7)

(sca-def-row sca-row-concat2 (a b) (concatenate 'string a b))
(sca-add-row "CONCATENATE-2" '((string 21)) #'sca-row-concat2 *sca-s* "abcde")

(sca-def-row sca-row-concat3 (a b c) (concatenate 'string a b c))
(sca-add-row "CONCATENATE-3" '((string 24)) #'sca-row-concat3 *sca-s* "abcde" "123")

(sca-def-row sca-row-upcase-subseq (s a b) (string-upcase (subseq s a b)))
(sca-add-row "STRING-UPCASE-SUBSEQ" '((string 4) (string 4))
             #'sca-row-upcase-subseq *sca-s* 3 7)

(sca-def-row sca-row-downcase-concat (a b) (string-downcase (concatenate 'string a b)))
(sca-add-row "STRING-DOWNCASE-CONCAT" '((string 21) (string 21))
             #'sca-row-downcase-concat *sca-s* "abcde")

(sca-def-row sca-row-princ-fixnum (x) (princ-to-string x))
(sca-add-row "PRINC-TO-STRING-FIXNUM" '((string 5)) #'sca-row-princ-fixnum 12345)

(sca-def-row sca-row-prin1-fixnum (x) (prin1-to-string x))
(sca-add-row "PRIN1-TO-STRING-FIXNUM" '((string 5)) #'sca-row-prin1-fixnum 12345)

(sca-def-row sca-row-princ-string (s) (princ-to-string s))
(sca-add-row "PRINC-TO-STRING-STRING" '((string 16)) #'sca-row-princ-string *sca-s*)

(sca-def-row sca-row-prin1-string (s) (prin1-to-string s))
(sca-add-row "PRIN1-TO-STRING-STRING" '((string 18)) #'sca-row-prin1-string *sca-s*)

(sca-def-row sca-row-write-hex (x) (write-to-string x :base 16))
(sca-add-row "WRITE-TO-STRING-BASE-16" '((string 4)) #'sca-row-write-hex 48879)

(sca-def-row sca-row-princ-symbol (x) (princ-to-string x))
(sca-add-row "PRINC-TO-STRING-SYMBOL" '((string 16)) #'sca-row-princ-symbol *sca-sym*)

(sca-def-row sca-row-prin1-character (c) (prin1-to-string c))
(sca-add-row "PRIN1-TO-STRING-CHARACTER" '((string 3)) #'sca-row-prin1-character #\a)

(sca-def-row sca-row-princ-ratio (x) (princ-to-string x))
(sca-add-row "PRINC-TO-STRING-RATIO" '((string 4)) #'sca-row-princ-ratio 2/3)

(sca-def-row sca-row-format-a-string (x) (format nil "~A" x))
(sca-add-row "FORMAT-A-STRING" '((string 16)) #'sca-row-format-a-string *sca-s*)

(sca-def-row sca-row-format-a-fixnum (x) (format nil "~A" x))
(sca-add-row "FORMAT-A-FIXNUM" '((string 5)) #'sca-row-format-a-fixnum 12345)

(sca-def-row sca-row-format-a-symbol (x) (format nil "~A" x))
(sca-add-row "FORMAT-A-SYMBOL" '((string 16)) #'sca-row-format-a-symbol *sca-sym*)

(sca-def-row sca-row-format-d-fixnum (x) (format nil "~D" x))
(sca-add-row "FORMAT-D-FIXNUM" '((string 5)) #'sca-row-format-d-fixnum 12345)

(sca-def-row sca-row-format-a-a (a b) (format nil "~A-~A" a b))
(sca-add-row "FORMAT-A-A" '((string 22)) #'sca-row-format-a-a *sca-s* 12345)

(sca-def-row sca-row-format-s-string (x) (format nil "~S" x))
(sca-add-row "FORMAT-S-STRING" '((string 18)) #'sca-row-format-s-string *sca-s*)

(sca-def-row sca-row-format-a-d (a b) (format nil "~A:~D" a b))
(sca-add-row "FORMAT-A-D" '((string 22)) #'sca-row-format-a-d *sca-s* 12345)

(sca-def-row sca-row-format-x () (format nil "x"))
(sca-add-row "FORMAT-CONSTANT" '((string 1)) #'sca-row-format-x)

(sca-def-row sca-row-parse-integer (s) (parse-integer s))
(sca-add-row "PARSE-INTEGER" 0 #'sca-row-parse-integer "12345")

(sca-def-row sca-row-parse-integer-bounds (s a b) (parse-integer s :start a :end b))
(sca-add-row "PARSE-INTEGER-BOUNDS" 0 #'sca-row-parse-integer-bounds *sca-num* 4 9)

(sca-def-row sca-row-parse-integer-radix (s) (parse-integer s :radix 16))
(sca-add-row "PARSE-INTEGER-RADIX" 0 #'sca-row-parse-integer-radix "beef")

(sca-def-row sca-row-parse-integer-junk-ok (s) (parse-integer s :junk-allowed t))
(sca-add-row "PARSE-INTEGER-JUNK-OK" 0 #'sca-row-parse-integer-junk-ok "12345xyz")

(sca-def-row sca-row-parse-integer-junk-none (s) (parse-integer s :junk-allowed t))
(sca-add-row "PARSE-INTEGER-JUNK-NONE" 0 #'sca-row-parse-integer-junk-none "xy12345")

(sca-def-row sca-row-intern-existing (s) (intern s))
(sca-add-row "INTERN-EXISTING" 0 #'sca-row-intern-existing *sca-name*)

(sca-def-row sca-row-intern-package (s p) (intern s p))
(sca-add-row "INTERN-PACKAGE" 0 #'sca-row-intern-package *sca-name* *sca-pkg*)

(sca-def-row sca-row-find-symbol (s) (find-symbol s))
(sca-add-row "FIND-SYMBOL" 0 #'sca-row-find-symbol *sca-name*)

(sca-def-row sca-row-symbol-name (x) (symbol-name x))
(sca-add-row "SYMBOL-NAME" 0 #'sca-row-symbol-name *sca-sym*)

(sca-def-row sca-row-intern-symbol-name (s) (symbol-name (intern s)))
(sca-add-row "SYMBOL-NAME-INTERN" 0 #'sca-row-intern-symbol-name *sca-name*)

(sca-def-row sca-row-string-symbol (x) (string x))
(sca-add-row "STRING-SYMBOL" 0 #'sca-row-string-symbol *sca-sym*)

(sca-def-row sca-row-make-symbol (s) (make-symbol s))
(sca-add-row "MAKE-SYMBOL" '((symbol 16)) #'sca-row-make-symbol *sca-name*)

(sca-def-row sca-row-chain-parse-subseq (s a b) (parse-integer (subseq s a b)))
(sca-add-row "CHAIN-PARSE-INTEGER-SUBSEQ" '((string 5))
             #'sca-row-chain-parse-subseq *sca-num* 4 9)

(sca-def-row sca-row-chain-concat-princ (k num)
  (concatenate 'string (symbol-name k) "-" (princ-to-string num)))
(sca-add-row "CHAIN-CONCAT-SYMBOL-NAME-PRINC" '((string 5) (string 22))
             #'sca-row-chain-concat-princ *sca-sym* 12345)

(sca-def-row sca-row-chain-format-a-a (a b) (format nil "~A-~A" a b))
(sca-add-row "CHAIN-FORMAT-A-A" '((string 22)) #'sca-row-chain-format-a-a *sca-s* 12345)

(defun sca-best (thunk &optional (n *sca-runs*)) (min-allocated-bytes thunk n))

(defun sca-empty-loop (n) (dotimes (i n) (setq *sca-sink* nil)) *sca-sink*)

(defun sca-mk-string (len n)
  (dotimes (i n) (setq *sca-sink* (make-string len))) *sca-sink*)

(defun sca-mk-symbol (len n)
  (let ((name (make-string len :initial-element #\a)))
    (dotimes (i n) (setq *sca-sink* (make-symbol name)))
    *sca-sink*))

(defun sca-bytes-per-call (total)
  (/ (- total *sca-baseline*) (float *sca-iterations*)))

(defun sca-floor-bytes (spec)
  (if (eql spec 0)
      0
      (let ((len (cadr spec)))
        (or (cdr (assoc spec *sca-floor-cache* :test #'equal))
            (let ((v (sca-bytes-per-call
                      (sca-best (lambda ()
                                  (case (car spec)
                                    (string (sca-mk-string len *sca-iterations*))
                                    (symbol (sca-mk-symbol len *sca-iterations*))))))))
              (push (cons spec v) *sca-floor-cache*)
              v)))))

(defun sca-floor (spec)
  (if (listp spec)
      (if (and spec (symbolp (car spec)))
          (sca-floor-bytes spec)
          (reduce #'+ spec :key #'sca-floor-bytes))
      (sca-floor-bytes spec)))

(defun sca-measure (fn args &optional (floor 0))
  ;; The measured code is the JIT's: until a method has been called enough and the tier-1
  ;; compile has had its delay, it allocates more (boxes the tier-0 code keeps). Earlier tests
  ;; used to warm these paths; a test run alone, or a shard of the suite, has not, so warm the
  ;; row here: call it for a while, past the tiering delay. On a busy machine the tier-1 compile
  ;; (a background thread) can take much longer than that, so a row that still allocates more
  ;; than its floor is warmed and measured again, for up to a few seconds: a row that really
  ;; allocates too much does so on every round and still fails, one that was only slow to
  ;; tier up settles to its floor.
  (let ((argsn (append args (list *sca-iterations*)))
        (give-up (+ (get-internal-real-time) (* 4 internal-time-units-per-second)))
        (got nil))
    (loop
      (let ((warm-until (+ (get-internal-real-time) (floor internal-time-units-per-second 7))))
        (loop do (apply fn argsn) until (>= (get-internal-real-time) warm-until)))
      (let ((now (sca-bytes-per-call (sca-best (lambda () (apply fn argsn))))))
        (setq got (if got (min got now) now)))
      (when (or (<= got (+ floor 16)) (>= (get-internal-real-time) give-up))
        (return got)))))

(defun sca-runtime-info ()
  "What a failure on one machine needs to be told from one on another: the runtime, the GC mode,
the processor count and what a one-character row allocates on its own."
  (list (dotnet:static "System.Runtime.InteropServices.RuntimeInformation" "FrameworkDescription")
        (list :server-gc (dotnet:static "System.Runtime.GCSettings" "IsServerGC"))
        (list :processors (dotnet:static "System.Environment" "ProcessorCount"))
        (list :baseline *sca-baseline*)))

(defun sca-allocation-failures ()
  (let ((*print-base* 10) (*print-radix* nil) (*print-pretty* nil)
        (*print-circle* nil) (*print-readably* nil) (*print-case* :upcase))
    (setq *sca-baseline* (sca-best (lambda () (sca-empty-loop *sca-iterations*))))
    (let ((bad nil))
      (dolist (row (reverse *sca-rows*))
        (destructuring-bind (label floor-spec fn args) row
          (let* ((floor (sca-floor floor-spec))
                 (got (let ((*package* *sca-pkg*)) (sca-measure fn args floor))))
            (when (> got (+ floor 16))
              (push (list label got floor) bad)))))
      (and bad (nconc (nreverse bad) (list (sca-runtime-info)))))))

(deftest-compiled-only string-chain-allocation.rows-at-floor
  (sca-allocation-failures)
  nil)
