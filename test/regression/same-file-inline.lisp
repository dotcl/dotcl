;;; COMPILE-FILE substitutes a small DEFUN earlier in the same file at its
;;; calls (CLHS 3.2.2.3), behind a check that the name still holds the function
;;; the file defined, so a later redefinition is seen as before. Not for a
;;; NOTINLINE name, a recursive or large body, a lambda list with more than
;;; required parameters, an OPTIMIZE declaration, a loop, LOAD-TIME-VALUE or a
;;; structured literal in the body, or under (DEBUG 3).

(defvar *sfi-sil* nil)

(defun %sfi-load ()
  (let ((fasl (regression-temp-file "same-file-inline.fasl")))
    (compile-file "test/regression/same-file/inline.lisp" :output-file fasl :sil t)
    (with-open-file (s (make-pathname :type "sil" :defaults fasl))
      (setf *sfi-sil* (make-string (file-length s)))
      (setf *sfi-sil* (subseq *sfi-sil* 0 (read-sequence *sfi-sil* s))))
    (load fasl)))

(when (emitting-mode-p) (%sfi-load))

(defun %sfi-substituted-p (name)
  (and (search (format nil "(:SAME-FILE-IS \"COMMON-LISP-USER\" ~s)" name) *sfi-sil*) t))

(deftest-emitting-only same-file-inline.substituted
  (mapcar #'%sfi-substituted-p '("%SFI-HELD-P" "%SFI-ADD1" "%SFI-TWO" "%SFI-LEN"))
  (t t t t))

(deftest-emitting-only same-file-inline.not-substituted
  (mapcar #'%sfi-substituted-p
          '("%SFI-NI" "%SFI-FACT" "%SFI-BIG" "%SFI-KEY" "%SFI-OPT" "%SFI-D3"
            "%SFI-LOOP" "%SFI-SPEED" "%SFI-LITERAL" "%SFI-CELL" "%SFI-LATER"
            "%SFI-FREE-VAR" "%SFI-TASK-HOLDING"))
  (nil nil nil nil nil nil nil nil nil nil nil nil nil))

(deftest-emitting-only same-file-inline.results
  (let ((tasks (list (funcall 'make-%sfi-task :pending t :waiting t)
                     (funcall 'make-%sfi-task :waiting t)
                     (funcall 'make-%sfi-task :holding t))))
    (list (funcall '%sfi-count-held tasks)
          (eq (funcall '%sfi-first-held tasks) (second tasks))
          (funcall '%sfi-twice-add1 3) (funcall '%sfi-tail-add1 4)
          (funcall '%sfi-mv 1 2) (funcall '%sfi-len-caller '(a b c))
          (funcall '%sfi-ni-caller 1) (funcall '%sfi-fact-caller 5)
          (funcall '%sfi-big-caller 1) (funcall '%sfi-key-caller 1)
          (funcall '%sfi-opt-caller 1) (funcall '%sfi-d3-caller 1)
          (funcall '%sfi-loop-caller 4) (funcall '%sfi-speed-caller 1)
          (progn (setf (symbol-value '%sfi-free-var) :global) (funcall '%sfi-free-caller))
          (eq (funcall '%sfi-literal-caller) (funcall '%sfi-literal))
          (eq (funcall '%sfi-cell-caller) (funcall '%sfi-cell))
          (funcall '%sfi-later-caller 1)))
  (2 t 8 5 (2 1) 3 2 120 15 3 2 2 6 2 (:lexical :global) t t (:later 1)))

(deftest-emitting-only same-file-inline.type-error
  (handler-case (progn (funcall '%sfi-len-caller 5) :no-error)
    (type-error () :type-error))
  :type-error)

(deftest-emitting-only same-file-inline.redefined-in-the-file
  (list (funcall '%sfi-redef-early 1) (funcall '%sfi-redef-late 2))
  ((:second 1) (:second 2)))

(deftest-emitting-only same-file-inline.redefined-after-load
  (progn
    (eval '(defun %sfi-add1 (x) (+ x 100)))
    (eval '(defun %sfi-held-p (task) (declare (ignore task)) t))
    (list (funcall '%sfi-twice-add1 3) (funcall '%sfi-tail-add1 4)
          (funcall '%sfi-count-held (list (funcall 'make-%sfi-task)))))
  (206 104 1))

;;; A literal in the body can look like nested code, ((:KEYWORD ...)), and be a
;;; dotted list; sizing the body for substitution used to walk into it and
;;; fail with a TYPE-ERROR. SBCL's MAKE-MACRO-LAMBDA quotes such a name.
(deftest-emitting-only same-file-inline.dotted-literal
  (let ((src (regression-temp-file "sfi-dotted.lisp"))
        (fasl (regression-temp-file "sfi-dotted.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defun %sfi-dotted (x) (list x '((:macro foo . bar))))
(defun %sfi-dotted-caller () (%sfi-dotted 1))
" s))
    (compile-file src :output-file fasl)
    (load fasl)
    (funcall '%sfi-dotted-caller))
  (1 ((:macro foo . bar))))
