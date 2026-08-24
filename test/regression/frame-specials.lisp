;;; DOTCL:FRAME-SPECIALS — the dynamic (special-variable) bindings in effect, for
;;; the debugger. The counterpart of FRAME-LOCALS, which shows only lexicals: a
;;; frame view without specials is half a view, since (let ((*x* 1)) ...) puts its
;;; value on the dynamic binding stack, not in a slot.
;;;
;;; Entries are ((SYMBOL value . own-p) ...), innermost first. OWN-P marks the
;;; bindings the named frame (or something it called) established. Unlike
;;; FRAME-LOCALS this does not need DOTCL:*EMIT-FRAME-LOCALS*: the binding stack
;;; always exists, and the frame index only decides where the OWN-P line falls.

(defvar *fs-outer* :fs-default)
(defvar *fs-inner* :fs-default)

(defun fs-entries (n)
  (mapcar (lambda (e) (list (car e) (cadr e) (if (cddr e) :own :caller)))
          (dotcl:frame-specials n)))

(defun fs-value (name entries)
  (cadr (find name entries :key (lambda (e) (symbol-name (car e))) :test #'string=)))

;;; Innermost first, with the value each binding holds.
(defun fs-collect () (fs-entries 0))

;; Filtered to this file's variables: whatever else the caller has bound is on
;; the stack too, and the order among ours is what this asserts.
(defun fs-ours (entries)
  (remove-if-not (lambda (e) (member (symbol-name (car e)) '("*FS-OUTER*" "*FS-INNER*")
                                     :test #'string=))
                 entries))

(deftest frame-specials-innermost-first
  (let ((*fs-outer* :by-outer))
    (let ((*fs-inner* :by-inner))
      (mapcar #'car (fs-ours (fs-collect)))))
  (*fs-inner* *fs-outer*))

(deftest frame-specials-values
  (let ((*fs-outer* :by-outer))
    (let ((*fs-inner* :by-inner))
      (let ((es (fs-collect)))
        (list (fs-value "*FS-INNER*" es) (fs-value "*FS-OUTER*" es)))))
  (:by-inner :by-outer))

;;; A rebinding of the same symbol does not hide the one it shadows: both are on
;;; the stack, and a reader of nested LETs wants to see that.
(deftest frame-specials-shadowed-binding-still-listed
  (let ((*fs-outer* 1))
    (let ((*fs-outer* 2))
      (mapcar #'cadr (remove-if-not (lambda (e) (string= (symbol-name (car e)) "*FS-OUTER*"))
                                    (fs-collect)))))
  (2 1))

;;; Attribution: with frame recording on, the frame that established a binding
;;; sees it as its own, and its callees see it as their caller's.
(setf dotcl:*emit-frame-locals* t)

;; Calls FRAME-SPECIALS directly: a helper in between would be a frame of its
;; own and shift every index by one.
(defun fs-probe (n)
  (mapcar (lambda (e) (list (car e) (cadr e) (if (cddr e) :own :caller)))
          (dotcl:frame-specials n)))
(defun fs-deep () (let ((*fs-inner* :by-deep)) (list (fs-probe 0) (fs-probe 1))))

(defun fs-run ()
  (let ((*fs-outer* :by-toplevel)) (fs-deep)))

;;; See frame-locals.lisp: the per-frame recording is emitted IL, so the case that
;;; asks which frame a binding belongs to is compiled-only.

(deftest-compiled-only frame-specials-own-vs-caller
  (progn
    (destructuring-bind (frame0 frame1) (fs-run)
      (list (third (find "*FS-INNER*" frame0 :key (lambda (e) (symbol-name (car e))) :test #'string=))
            (third (find "*FS-INNER*" frame1 :key (lambda (e) (symbol-name (car e))) :test #'string=))
            (third (find "*FS-OUTER*" frame1 :key (lambda (e) (symbol-name (car e))) :test #'string=)))))
  ;; frame 0 is the probe itself — *fs-inner* was bound before it started.
  ;; frame 1 is FS-DEEP, which bound it. *fs-outer* belongs to the caller either way.
  (:caller :own :caller))

;;; An out-of-range frame index still answers with the bindings in effect; only
;;; the OWN-P split is unavailable.
(deftest frame-specials-unknown-frame-still-lists
  (let ((*fs-inner* :x))
    (let ((es (fs-entries 999)))
      (list (fs-value "*FS-INNER*" es)
            (third (find "*FS-INNER*" es :key (lambda (e) (symbol-name (car e))) :test #'string=)))))
  (:x :caller))

;;; Outside any binding form the stack is empty of these names.
(deftest frame-specials-none-of-ours-when-unbound
  (remove-if-not (lambda (e) (member (symbol-name (car e)) '("*FS-OUTER*" "*FS-INNER*") :test #'string=))
                 (fs-collect))
  ())

;;; Back to the default. The flag is global and drives codegen at LOAD time, so
;;; leaving it on here compiles every test file loaded after this one with frame
;;; bookkeeping -- the two thirds of the suite past this point would then test
;;; instrumented code and never the code a default build produces.
(setf dotcl:*emit-frame-locals* nil)
