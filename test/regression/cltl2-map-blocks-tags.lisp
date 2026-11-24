;;; DOTCL-CLTL2:MAP-ENVIRONMENT-BLOCKS and MAP-ENVIRONMENT-TAGS list the blocks
;;; and TAGBODY tags around the call of a macro, from its &ENVIRONMENT: innermost
;;; first, each name once, called with (NAME KIND DECLARATIONS), the shape
;;; MAP-ENVIRONMENT-VARIABLES and -FUNCTIONS use (KIND is :BLOCK / :TAG,
;;; DECLARATIONS NIL). An environment that carries no lexical state (NIL) is
;;; inside nothing, and nothing is called. Compiled code only: the interpreter's
;;; environments carry no compile state.

(setf dotcl:*save-sil* t)

(defmacro mbt-blocks (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-blocks
     (lambda (name kind decls) (push (list name kind decls) out))
     env)
    `',(nreverse out)))

(defmacro mbt-tags (&environment env)
  (let ((out '()))
    (dotcl-cltl2:map-environment-tags
     (lambda (name kind decls) (push (list name kind decls) out))
     env)
    `',(nreverse out)))

;; Expands to (RETURN-FROM NAME VALUE) when NAME is a block in scope, as a code
;; walker needs to know, else to the symbol :NO-BLOCK.
(defmacro mbt-exit (name value &environment env)
  (let ((found nil))
    (dotcl-cltl2:map-environment-blocks
     (lambda (n kind decls) (declare (ignore kind decls)) (when (eq n name) (setq found t)))
     env)
    (if found `(return-from ,name ,value) :no-block)))

(defmacro mbt-go (tag &environment env)
  (let ((found nil))
    (dotcl-cltl2:map-environment-tags
     (lambda (n kind decls) (declare (ignore kind decls)) (when (eq n tag) (setq found t)))
     env)
    (if found `(go ,tag) :no-tag)))

(deftest-compiled-only cltl2-map-blocks.nested-innermost-first
  (block outer (block inner (mbt-blocks)))
  ((inner :block nil) (outer :block nil)))

(deftest-compiled-only cltl2-map-blocks.same-name-once
  (block b (block b (mbt-blocks)))
  ((b :block nil)))

(deftest-compiled-only cltl2-map-blocks.not-inside-after-block
  (list (block b 1) (mbt-blocks))
  (1 ()))

(deftest-compiled-only cltl2-map-blocks.macrolet
  (block outer
    (macrolet ((m () '(block inner (mbt-blocks))))
      (m)))
  ((inner :block nil) (outer :block nil)))

;; A local function's own implicit block (CLHS 5.3 FLET) is in scope in its body.
(deftest-compiled-only cltl2-map-blocks.flet-and-labels
  (block outer
    (flet ((f () (block in-flet (mbt-blocks))))
      (labels ((g () (block in-labels (mbt-blocks))))
        (list (mapcar #'car (f)) (mapcar #'car (g))))))
  ((in-flet f outer) (in-labels g outer)))

(deftest-compiled-only cltl2-map-blocks.nil-block-of-loop
  (dolist (x '(1))
    (return (mbt-blocks)))
  ((nil :block nil)))

(deftest-compiled-only cltl2-map-blocks.return-from-in-expansion
  (block blk (list (mbt-exit blk 20) 30))
  20)

(deftest-compiled-only cltl2-map-blocks.no-block-in-scope
  (block other (mbt-exit blk 20))
  :no-block)

(deftest-compiled-only cltl2-map-blocks.return-from-in-flet-body
  (block blk
    (flet ((f () (mbt-exit blk 7)))
      (f)
      8))
  7)

(deftest-compiled-only cltl2-map-blocks.empty-environment
  (let ((called 0))
    (dotcl-cltl2:map-environment-blocks (lambda (&rest args) (declare (ignore args)) (incf called)) nil)
    (dotcl-cltl2:map-environment-tags (lambda (&rest args) (declare (ignore args)) (incf called)) nil)
    called)
  0)

(deftest-compiled-only cltl2-map-blocks.augmented-environment-keeps-blocks
  (block b
    (macrolet ((m (&environment env)
                 (let ((env2 (dotcl-cltl2:augment-environment env :variable '(zz)))
                       (out '()))
                   (dotcl-cltl2:map-environment-blocks (lambda (n k d) (declare (ignore k d)) (push n out)) env2)
                   `',(nreverse out))))
      (m)))
  (b))

(deftest-compiled-only cltl2-map-tags.nested-innermost-first
  (block r
    (tagbody
     outer-tag
       (tagbody
        inner-tag
          (return-from r (mbt-tags)))))
  ((inner-tag :tag nil) (outer-tag :tag nil)))

(deftest-compiled-only cltl2-map-tags.integer-tags-and-same-name-once
  (block r
    (tagbody
     1
       (tagbody
        2
          (tagbody
           1
             (return-from r (mbt-tags))))))
  ((1 :tag nil) (2 :tag nil)))

(deftest-compiled-only cltl2-map-tags.not-inside-after-tagbody
  (list (tagbody tag1 nil) (mbt-tags))
  (nil ()))

(deftest-compiled-only cltl2-map-tags.macrolet-and-flet
  (block r
    (tagbody
     a-tag
       (macrolet ((m () '(flet ((f () (mapcar #'car (mbt-tags)))) (return-from r (f)))))
         (m))))
  (a-tag))

(deftest-compiled-only cltl2-map-tags.go-in-expansion
  (let ((n 0))
    (tagbody
     again
       (incf n)
       (when (< n 3) (mbt-go again)))
    n)
  3)

(deftest-compiled-only cltl2-map-tags.no-tag-in-scope
  (block r (tagbody other (return-from r (mbt-go gone))))
  :no-tag)

;; A MACROLET macro whose &ENVIRONMENT goes to a function is not expanded by the
;; analysis walks, which keep no block or tag names: it runs once, where the form
;; is compiled, and there it sees them. (A walker called from the expander would
;; otherwise run twice, the first time without the blocks.)
(defvar *mbt-runs* 0)

(defun mbt-helper (env)
  (incf *mbt-runs*)
  (let ((out '()))
    (dotcl-cltl2:map-environment-blocks (lambda (name kind decls) (declare (ignore kind decls)) (push name out)) env)
    (nreverse out)))

(deftest-compiled-only cltl2-map-blocks.macrolet-env-to-function-runs-once
  (let ((*mbt-runs* 0))
    (list (funcall (compile nil '(lambda ()
                                  (block outer
                                    (block inner
                                      (macrolet ((m (&environment e) `',(mbt-helper e)))
                                        (m)))))))
          *mbt-runs*))
  ((inner outer) 1))

;; The environment given only to MACROEXPAND is still walked: the expansion's
;; variable and type facts reach the typed paths (no boxing of ACC below).
(defun mbt-contained (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (macrolet ((m (x &environment e) (macroexpand x e) `(setq acc (logand (+ acc ,x) 1073741823))))
      (dotimes (i n) (m i)))
    acc))

(deftest-emitting-only cltl2-map-blocks.macrolet-env-to-macroexpand-not-boxed
  (let ((sil (dotcl:function-sil #'mbt-contained)))
    (count-if (lambda (i) (and (consp i) (stringp (cadr i)) (search "Box" (cadr i)))) sil))
  0)

;; The names reach a macro in any position, also where the compiler does not
;; compile a block at all (nothing returns from it) and inside a local function.
(defmacro mbt-block-names (&environment env)
  `',(mapcar #'car (second (macroexpand-1 '(mbt-blocks) env))))

(defmacro mbt-tag-names (&environment env)
  `',(mapcar #'car (second (macroexpand-1 '(mbt-tags) env))))

(defvar *mbt-sink* nil)

(defun mbt-setq-block ()
  (let ((r nil)) (block b (setq r (mbt-block-names))) r))
(defun mbt-setq-block-list ()
  (let ((r nil)) (block b (setq r (list (mbt-block-names)))) r))
(defun mbt-setf-block ()
  (let ((r nil)) (block b (setf r (mbt-block-names))) r))
(defun mbt-psetq-block ()
  (let ((r nil)) (block b (psetq r (mbt-block-names))) r))
(defun mbt-global-setq-block ()
  (block b (setq *mbt-sink* (mbt-block-names))) *mbt-sink*)
(defun mbt-dolist-setq-block ()
  (let ((r nil)) (dolist (x '(1)) (block b (setq r (mbt-block-names)))) r))
(defun mbt-dotimes-setq-block ()
  (let ((r nil)) (dotimes (i 1) (block b (setq r (mbt-block-names)))) r))
(defun mbt-loop-setq-block ()
  (let ((r nil)) (loop repeat 1 do (block b (setq r (mbt-block-names)))) r))
(defun mbt-let-init-block ()
  (let ((r (block b (mbt-block-names)))) r))
(defun mbt-setq-tag ()
  (let ((r nil)) (tagbody t1 (setq r (mbt-tag-names))) r))
(defun mbt-flet-tags ()
  (tagbody t1
     (flet ((g () (mbt-tag-names)))
       (return-from mbt-flet-tags (g)))))
(defun mbt-labels-tags ()
  (tagbody t1
     (labels ((g () (mbt-tag-names)))
       (return-from mbt-labels-tags (g)))))
(defun mbt-flet-blocks ()
  (block b
    (flet ((g () (mbt-block-names)))
      (return-from mbt-flet-blocks (g)))))

(deftest-compiled-only cltl2-map-blocks.setq-value-position
  (list (mbt-setq-block) (mbt-setq-block-list) (mbt-setf-block) (mbt-psetq-block)
        (mbt-global-setq-block) (mbt-let-init-block))
  ((b mbt-setq-block) ((b mbt-setq-block-list)) (b mbt-setf-block) (b mbt-psetq-block)
   (b mbt-global-setq-block) (b mbt-let-init-block)))

(deftest-compiled-only cltl2-map-blocks.setq-value-position-in-loops
  (list (mbt-dolist-setq-block) (mbt-dotimes-setq-block) (mbt-loop-setq-block))
  ((b nil mbt-dolist-setq-block) (b nil mbt-dotimes-setq-block) (b nil mbt-loop-setq-block)))

(deftest-compiled-only cltl2-map-tags.setq-value-position
  (mbt-setq-tag)
  (t1))

(deftest-compiled-only cltl2-map-tags.local-function-body
  (list (mbt-flet-tags) (mbt-labels-tags) (mbt-flet-blocks))
  ((t1) (t1) (g b mbt-flet-blocks)))
