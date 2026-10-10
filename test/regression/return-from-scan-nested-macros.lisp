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

;; Only the expansion of a macro call is code. An argument the macro drops is
;; never compiled, so the scan must not expand the macro calls inside it either:
;; their expanders can have compile-time side effects. serapeum's dispatch-case
;; drops the clauses that an earlier clause shadows, and expanding them anyway
;; made its exhaustiveness check warn about branches that do not exist.
(defvar *rfs-dropped-expansions* 0)
(defmacro rfs-counted () (incf *rfs-dropped-expansions*) nil)
(defmacro rfs-drop-first (&rest forms) `(progn ,@(rest forms)))

(deftest-compiled-only return-from-scan.dropped-argument-not-expanded
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "rfs-drop.lisp" dir))
         (fasl (merge-pathnames "rfs-drop.fasl" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defun rfs-drop-f1 () (cl-user::rfs-drop-first (cl-user::rfs-counted) 1))" s)
      (write-line "(defun rfs-drop-f2 (x) (let ((y x)) (lambda () (cl-user::rfs-drop-first (cl-user::rfs-counted) y))))" s))
    (setf *rfs-dropped-expansions* 0)
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    *rfs-dropped-expansions*)
  0)
