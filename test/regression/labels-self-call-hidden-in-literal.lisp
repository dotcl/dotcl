;;; A LABELS definition whose self-call is produced by a macro from data the
;;; source walk cannot see.
;;;
;;; A single LABELS definition that never names itself is compiled like an FLET.
;;; That check looks at the source, so a self-call hidden in a literal the walk
;;; does not descend into (here a vector; in SBCL's own sources, the structure
;;; its backquote reader makes for ,X) was compiled as a call to the global
;;; function of that name: "Undefined function". SBCL's ONCE-ONLY is written
;;; this way (a NAMED-LET whose recursion sits under ,,), and building SBCL
;;; with dotcl as the host stopped on it.

(defmacro rtl-call (v &rest args) `(,(aref v 0) ,@args))
(defmacro rtl-fn (v) `(function ,(aref v 0)))

(deftest labels-self-call-hidden-in-literal
  (labels ((rtl-f (n) (if (= n 0) :done (rtl-call #(rtl-f) (1- n)))))
    (rtl-f 3))
  :done)

(deftest labels-self-function-hidden-in-literal
  (labels ((rtl-f (n) (if (= n 0) :done (funcall (rtl-fn #(rtl-f)) (1- n)))))
    (rtl-f 3))
  :done)

;; The same shape as ONCE-ONLY: the recursion is only reachable through a
;; nested backquote.
(defmacro rtl-once-only (specs &body body)
  (labels ((rtl-frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let* ((spec (first specs))
                        (name (first spec))
                        (tmp (gensym "ONCE-ONLY")))
                   `(let ((,tmp ,(second spec))
                          (,name (gensym ,(symbol-name name))))
                      `(let ((,,name ,,tmp))
                         ,,(rtl-call #(rtl-frob) (rest specs) body)))))))
    (rtl-frob specs body)))

(defmacro rtl-twice (x) (rtl-once-only ((y x)) `(+ ,y ,y)))

(deftest labels-self-call-hidden-once-only
  (let ((n 0)) (list (rtl-twice (incf n)) n))
  (2 1))

;; A self-free LABELS is still compiled the cheap way and still works.
(deftest labels-self-free-still-works
  (labels ((rtl-g (x) (* x 2))) (rtl-g 21))
  42)
