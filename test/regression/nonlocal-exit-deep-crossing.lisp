;;; A non-local GO or RETURN-FROM crossing many levels must not cost stack per
;;; level. Same defect CATCH, HANDLER-CASE and RESTART-CASE each had before their
;;; tag tests moved into filters: TAGBODY and BLOCK caught every GoException /
;;; BlockReturnException and rethrew the ones whose tag belonged to an outer
;;; level. A rethrow restarts exception dispatch from inside the handler funclet,
;;; and that funclet stays live for the rest of the exception's journey, so
;;; crossing N levels stacked N dispatches: 20000 levels took ~4s and 50000 killed
;;; the process as an uncatchable .NET StackOverflowException. Matching the tag in
;;; a CIL exception FILTER lets a level that owns nothing decline without being
;;; entered.
;;;
;;; Both forms emit the catch only when a non-local GO / RETURN-FROM exists, so
;;; each level below closes over one to make the crossed frames real.
;;;
;;; Compiled-only: these depths are about the compiled frame layout, and one
;;; interpreted level costs tens of .NET frames, so the recursion cannot reach them
;;; without a compiler.

(defvar *nld-go-out* nil)
(defvar *nld-val* 0)

(defun %nld-tb-rec (n)
  (tagbody
     (if (zerop n)
         (funcall *nld-go-out*)                  ; targets the OUTERMOST tagbody
         (progn (setq *nld-val* (+ 1 (%nld-tb-rec (- n 1)))) (go done)))
     ;; a non-local go, captured: this is what makes the level emit its catch
     (funcall (lambda () (go done)))
   done)
  *nld-val*)

(defun %nld-run-tb (d)
  (let ((res :none))
    (tagbody
       (let ((*nld-go-out* (lambda () (go out))))
         (%nld-tb-rec d))
       (go end)
     out
       (setq res :went-out)
     end)
    res))

(deftest-compiled-only nonlocal-exit-deep-crossing.go-crosses-nested-tagbodies
  (%nld-run-tb 50000)
  :went-out)

(defvar *nld-blk-out* nil)

(defun %nld-blk-rec (n)
  (block lvl
    (let ((esc (lambda () (return-from lvl :inner))))   ; forces this level's catch
      (declare (ignorable esc))
      (if (zerop n)
          (funcall *nld-blk-out*)                       ; targets the OUTERMOST block
          (+ 1 (%nld-blk-rec (- n 1)))))))

(defun %nld-run-blk (d)
  (block outer
    (let ((*nld-blk-out* (lambda () (return-from outer :returned-outside))))
      (%nld-blk-rec d))))

(deftest-compiled-only nonlocal-exit-deep-crossing.return-from-crosses-nested-blocks
  (%nld-run-blk 50000)
  :returned-outside)

;;; The level that OWNS the tag still takes it, and the innermost owner wins: each
;;; level here escapes to its own block, so the value is the level's own marker and
;;; not an outer level's.

(defun %nld-blk-own (n)
  (block lvl
    (let ((esc (lambda () (return-from lvl (list :own n)))))
      (if (zerop n) (funcall esc) (cons n (%nld-blk-own (- n 1)))))))

(deftest nonlocal-exit-deep-crossing.owning-level-still-takes-it
  (%nld-blk-own 3)
  (3 2 1 :own 0))

;;; A GO that lands on its own tagbody's tag keeps working across a crossing: the
;;; filter must not disturb the ordinary loop case.

(defun %nld-tb-own (n)
  (let ((acc 0))
    (tagbody
     top
       (funcall (lambda () (when (> n 0) (decf n) (incf acc) (go top)))))
    acc))

(deftest nonlocal-exit-deep-crossing.non-local-go-to-own-tag-loops
  (%nld-tb-own 100)
  100)
