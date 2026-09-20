;;; (OPTIMIZE (DEBUG N)) from a body outranks the one DECLAIM put in force.
;;;
;;; A function compiled under DEBUG 0 records no debugger frame, so it does not
;;; appear in a backtrace taken below it -- that is the saving, and it is what
;;; these tests read. The global value was honoured in one direction only: a
;;; file-level (declaim (optimize (debug 0))) turned frames off for everything
;;; after it, and a single function asking for (debug 3) could not get its own
;;; frame back, which is the one thing that declaration is for. CLHS 3.3.4 puts
;;; the body's declaration first whichever way it points.
;;;
;;; The subject is emitted code, so these are DEFTEST-COMPILED-ONLY: the
;;; tree-walk evaluator names the functions it builds regardless of the
;;; declaration, and the assertion below would say nothing there.
;;;
;;; This file is loaded just before interp-backtrace-frames.lisp on purpose: if
;;; the DECLAIM here leaked past the end of the file, the frames that file
;;; asserts would be gone.

(defun %dod-probe () (dotcl:backtrace))

;;; ---- with nothing in force, a function has its frame ----

(defun %dod-default () (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.default-has-frame
  (and (member "%DOD-DEFAULT" (%dod-default) :test #'string=) t)
  t)

;;; ---- the global value applies to what is compiled after it ----

(declaim (optimize (debug 0)))

(defun %dod-after-declaim () (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.declaim-removes-frame
  (and (member "%DOD-AFTER-DECLAIM" (%dod-after-declaim) :test #'string=) t)
  nil)

;;; ---- and a body declaration outranks it, here putting the frame back ----

(defun %dod-body-debug-3 ()
  (declare (optimize (debug 3)))
  (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.body-declaration-wins
  (and (member "%DOD-BODY-DEBUG-3" (%dod-body-debug-3) :test #'string=) t)
  t)

;; A bare DEBUG (no value) means 3, so it puts the frame back too.
(defun %dod-body-bare-debug ()
  (declare (optimize debug))
  (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.bare-debug-wins
  (and (member "%DOD-BODY-BARE-DEBUG" (%dod-body-bare-debug) :test #'string=) t)
  t)

;;; ---- a later DECLAIM takes the global back ----

(declaim (optimize (debug 1)))

(defun %dod-after-restore () (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.later-declaim-restores
  (and (member "%DOD-AFTER-RESTORE" (%dod-after-restore) :test #'string=) t)
  t)

;;; ---- and the body still turns frames off under the restored global ----

(defun %dod-body-debug-0 ()
  (declare (optimize (debug 0)))
  (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.body-debug-0-still-opts-out
  (and (member "%DOD-BODY-DEBUG-0" (%dod-body-debug-0) :test #'string=) t)
  nil)

;; A docstring before the declaration does not hide it.
(defun %dod-docstring ()
  "Frames off, declared after the docstring."
  (declare (optimize (debug 0)))
  (%dod-probe))

(deftest-compiled-only declaim-optimize-debug.docstring-then-declaration
  (and (member "%DOD-DOCSTRING" (%dod-docstring) :test #'string=) t)
  nil)

;;; ---- the declaration changes the frame and nothing else ----

(defun %dod-value-framed (a b)
  (declare (optimize (debug 3)))
  (+ a b))

(defun %dod-value-frameless (a b)
  (declare (optimize (debug 0)))
  (+ a b))

(deftest declaim-optimize-debug.values-unchanged
  (list (%dod-value-framed 2 3) (%dod-value-frameless 2 3))
  (5 5))

;; The frame a function does record names that function and nothing else: the
;; probe's own frame is there either way, and only the caller's comes and goes.
(deftest-compiled-only declaim-optimize-debug.backtrace-shape
  (list (%dod-default) (%dod-body-debug-0))
  (("%DOD-PROBE" "%DOD-DEFAULT") ("%DOD-PROBE")))
