;;; The stream LOAD hands to a reader macro has to answer FILE-POSITION.
;;;
;;; It used to answer NIL: LOAD wrapped the source in a plain StringReader while
;;; COMPILE-FILE wrapped it in a position-tracking one. A reader macro that
;;; records source locations then had NIL where an offset belongs — eclector does
;;; this, and so does coalton (whose reader macro runs its entire front end), so
;;; every coalton form in a LOADed file died with "Not a number: NIL" while the
;;; same file went through COMPILE-FILE fine. The asymmetry is what made it look
;;; like a library problem rather than ours.
;;;
;;; CLHS 21.2 lets FILE-POSITION answer NIL only when the position cannot be
;;; determined; for a file being loaded it can.

(defvar *lsp-positions* nil)

(defun %lsp-write (path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    ;; Two reads, so a position that never advances is caught as well as a NIL one.
    (write-string "(defparameter cl-user::*lsp-a* #@)" s) (terpri s)
    (write-string "(defparameter cl-user::*lsp-b* #@)" s) (terpri s)))

(defun %lsp-case (op)
  (let* ((tmp (format nil "~a/dotcl-loadpos-~a"
                      (or (dotcl:getenv "TEMP") "/tmp")
                      (get-internal-real-time)))
         (src (format nil "~a/src.lisp" tmp)))
    (ensure-directories-exist (concatenate 'string tmp "/"))
    (%lsp-write src)
    (let ((*readtable* (copy-readtable)))
      (set-dispatch-macro-character
       #\# #\@
       (lambda (s c n)
         (declare (ignore c n))
         (push (file-position s) *lsp-positions*)
         0))
      (let ((*lsp-positions* nil))
        (ecase op
          (:load (load src))
          (:compile-file (compile-file src)))
        (let ((ps (reverse *lsp-positions*)))
          (list (length ps)
                (every #'integerp ps)
                ;; strictly increasing: the second read is further into the file
                (and (= (length ps) 2) (< (first ps) (second ps)))))))))

(deftest load-stream-position.load-reports-position
  (%lsp-case :load)
  (2 t t))

;;; COMPILE-FILE already did this; keep it next to LOAD so the two cannot drift
;;; apart again without a test noticing.
(deftest-compiled-only load-stream-position.compile-file-reports-position
  (%lsp-case :compile-file)
  (2 t t))
