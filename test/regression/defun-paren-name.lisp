;;; Regression: DEFUN of a symbol whose print-name starts with #\( must
;;; register the function like any other symbol. The mangled name string is
;;; used to distinguish compound function names — (SETF x), (CAS x) — which
;;; register on a target symbol's slot rather than as a symbol-function. That
;;; discriminator used StartsWith("(") alone, which also matched a plain symbol
;;; whose name merely starts with '(' (e.g. SB-FORMAT::|(-COMPILER|, the ~(
;;; format directive handler produced by directive-handler-name). Such a defun
;;; returned normally yet left the function unbound (FBOUNDP => NIL). The fix
;;; requires both a leading '(' and a trailing ')' for the compound classification.

(defun |(-paren-head| (x) (* x 2))

(deftest defun-paren-head-fboundp
  (fboundp '|(-paren-head|)
  t)

(deftest defun-paren-head-symbol-function
  (funcall (symbol-function '|(-paren-head|) 21)
  42)

(deftest defun-paren-head-sharp-quote
  (funcall #'|(-paren-head| 21)
  42)

;;; The exact shape SBCL's def-complex-format-directive emits for the ~(
;;; directive: (progn (defun NAME ...) (setf (aref v i) #'NAME) 'NAME).
(defvar *paren-expander-vec* (make-array 4 :initial-element nil))
(defun install-paren-expander ()
  (setf (aref *paren-expander-vec* 0) #'|(-paren-head|)
  (car (list t)))

(deftest defun-paren-head-aref-sharp-quote
  (progn (install-paren-expander)
         (funcall (aref *paren-expander-vec* 0) 21))
  42)

;;; A paren that is NOT leading must still work (it always did) — guard against
;;; over-broadening the compound classification.
(defun |mid(paren| (x) (* x 3))

(deftest defun-mid-paren-still-works
  (funcall '|mid(paren| 10)
  30)

;;; (SETF ...) compound names must still route to the SetfFunction slot.
(defun (setf |paren-place|) (v obj) (setf (car obj) v) v)

(deftest defun-setf-compound-still-works
  (let ((cell (list 0)))
    (funcall #'(setf |paren-place|) 99 cell)
    (car cell))
  99)
