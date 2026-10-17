;;; A RANDOM-STATE printed readably reads back, compiled and interpreted, and
;;; reading it interns nothing into COMMON-LISP.

(defun %rsp (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e) (princ-to-string e))))))

;;; --- it reads back (the first half of ansi PRINT.RANDOM-STATE.1)

(defparameter %rsp-round-trip
  '(let* ((rs1 (make-random-state *random-state*))
          (rs2 (with-standard-io-syntax
                 (read-from-string (write-to-string rs1 :readably t)))))
    (list (and (random-state-p rs2) t) (and (typep rs2 'random-state) t))))

(deftest random-state-readable.round-trip-compile
  (%rsp :compile %rsp-round-trip)
  (t t))

(deftest random-state-readable.round-trip-interpret
  (%rsp :interpret %rsp-round-trip)
  (t t))

;;; --- what was read back must produce the SAME sequence as the original (what
;;; ansi's is-similar checks). "read-from-string returned some random-state" is
;;; not enough.

(defparameter %rsp-same-sequence
  '(let* ((rs1 (make-random-state *random-state*))
          (rs2 (with-standard-io-syntax
                 (read-from-string (write-to-string rs1 :readably t))))
          (a (loop repeat 50 collect (random 16777215 (make-random-state rs1))))
          (b (loop repeat 50 collect (random 16777215 (make-random-state rs2)))))
    (equal a b)))

(deftest random-state-readable.same-sequence-compile
  (%rsp :compile %rsp-same-sequence)
  t)

(deftest random-state-readable.same-sequence-interpret
  (%rsp :interpret %rsp-same-sequence)
  t)

;;; --- reading it must not pollute COMMON-LISP

(deftest random-state-readable.no-cl-pollution-interpret
  (%rsp :interpret '(progn
                     (with-standard-io-syntax
                       (read-from-string (write-to-string (make-random-state) :readably t)))
                     (and (find-symbol "MAKE-RANDOM-STATE-FROM-SEEDS" "COMMON-LISP") t)))
  nil)

;;; --- over-fix guards: unreadable printing and RANDOM-STATE itself still work

(deftest random-state-readable.unreadable-print-interpret
  (%rsp :interpret '(let ((*print-readably* nil))
                     (and (search "RANDOM-STATE" (princ-to-string (make-random-state))) t)))
  t)

(deftest random-state-readable.random-still-works-interpret
  (%rsp :interpret '(let ((r (make-random-state t)))
                     (and (integerp (random 100 r))
                          (< (random 100 r) 100)
                          (random-state-p r))))
  t)
