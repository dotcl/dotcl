;;; The pre-pass that decides whether a DEFUN keeps its implicit block scans the
;;; body for a RETURN-FROM, expanding global macros. It looks at both a macro
;;; call's expansion and its original arguments, and the expansion embeds those
;;; same arguments, so nested macro calls used to be rescanned once per
;;; enclosing level: 2^depth. 3d-math's matrices/types.lisp (a 16-argument
;;; type dispatch) took minutes to compile because of it. The scan now
;;; remembers forms it has already found free of RETURN-FROM.

(defmacro rfs-wrap (&body body) `(rfs-inner (let () ,@body)))
(defmacro rfs-inner (form) `(progn ,form))

;; 30 levels: without memoization this is on the order of 2^30 visits.
(defmacro rfs-nest (n form)
  (if (zerop n) form `(rfs-wrap (rfs-nest ,(1- n) ,form))))

(defun rfs-deep-no-return (x) (rfs-nest 30 (+ x 1)))
(deftest return-from-scan.deep-nesting-no-return
  (rfs-deep-no-return 41)
  42)

;; A RETURN-FROM at the bottom of the same nest must still be found.
(defun rfs-deep-return (x) (rfs-nest 30 (return-from rfs-deep-return (* x 2))) 99)
(deftest return-from-scan.deep-nesting-return
  (rfs-deep-return 21)
  42)

;; The same subform reached first under a shadowing BLOCK of the same name and
;; then outside it: the negative answer inside the block must not be reused.
(defmacro rfs-ret-from-outer () `(return-from rfs-shadow 7))
(defmacro rfs-twice (form) `(progn (block rfs-shadow ,form) ,form))
(defun rfs-shadow () (rfs-twice (rfs-ret-from-outer)) 99)
(deftest return-from-scan.shadowing-block
  (rfs-shadow)
  7)
