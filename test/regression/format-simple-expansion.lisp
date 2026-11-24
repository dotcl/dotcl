;;; (FORMAT dest "literal control string of ~A ~S ~D ~% ~~" args) written as stream calls.
;;; Each control is compiled as a literal call (expanded) and also run through the FORMAT
;;; function with APPLY (not expanded); the outputs must be the same under every binding of the
;;; printer variables.

(defmacro %def-fmt-pair (name control nargs)
  (let ((vars (loop for i below nargs collect (intern (format nil "A~D" i)))))
    `(defun ,name (dest ,@vars) (format dest ,control ,@vars))))

(%def-fmt-pair %fs-a "~A" 1)
(%def-fmt-pair %fs-s "~S" 1)
(%def-fmt-pair %fs-d "~D" 1)
(%def-fmt-pair %fs-lit-d "a~Db" 1)
(%def-fmt-pair %fs-as "~A-~S" 2)
(%def-fmt-pair %fs-nl "x~%y" 0)
(%def-fmt-pair %fs-tilde "~~" 0)
(%def-fmt-pair %fs-dd "~D~D" 2)
(%def-fmt-pair %fs-trail "ab~%" 0)
(%def-fmt-pair %fs-lead "~%~A" 1)
(%def-fmt-pair %fs-dna "~d~a~s" 3)
(%def-fmt-pair %fs-text "no directives" 0)

(defparameter *fs-pairs*
  `((%fs-a "~A" 1) (%fs-s "~S" 1) (%fs-d "~D" 1) (%fs-lit-d "a~Db" 1) (%fs-as "~A-~S" 2)
    (%fs-nl "x~%y" 0) (%fs-tilde "~~" 0) (%fs-dd "~D~D" 2) (%fs-trail "ab~%" 0)
    (%fs-lead "~%~A" 1) (%fs-dna "~d~a~s" 3) (%fs-text "no directives" 0)))

(defparameter *fs-values*
  (list 0 5 -7 255 12345678901234567890 -12345678901234567890 "str" "" "a\"b" nil t 'sym :kw '|Mixed Case|
        1/3 -5/7 1.5 1.5d0 -0.0d0 1d20 #\a #\Space '(1 "a" #\b (2 3)) '(a . b) #(1 2 "x") "tab	x"))

(defun %fs-bindings (k)
  (ecase k
    (0 (lambda (thunk) (funcall thunk)))
    (1 (lambda (thunk) (let ((*print-base* 16)) (funcall thunk))))
    (2 (lambda (thunk) (let ((*print-base* 2) (*print-radix* t)) (funcall thunk))))
    (3 (lambda (thunk) (let ((*print-escape* nil)) (funcall thunk))))
    (4 (lambda (thunk) (let ((*print-pretty* t)) (funcall thunk))))
    (5 (lambda (thunk) (let ((*print-pretty* nil) (*print-circle* t)) (funcall thunk))))
    (6 (lambda (thunk) (let ((*print-readably* t)) (handler-case (funcall thunk) (error () :error)))))
    (7 (lambda (thunk) (let ((*print-case* :downcase)) (funcall thunk))))))

(defun %fs-run (fn args via-apply control)
  (let ((s (make-string-output-stream)))
    (let ((r (handler-case
                 (if via-apply
                     (apply #'format s control args)
                     (apply fn s args))
               (error (e) (type-of e)))))
      (list r (get-output-stream-string s)))))

(defun %fs-arg-lists (n)
  (case n
    (0 '(()))
    (1 (mapcar #'list *fs-values*))
    (2 (loop for a in *fs-values* for b in (append (cdr *fs-values*) (list (car *fs-values*)))
             collect (list a b)))
    (t (loop for a in *fs-values* for b in (cdr *fs-values*) for c in (cddr *fs-values*)
             collect (list a b c)))))

(deftest format-simple-expansion.same-as-function
  (let ((diffs '()))
    (dolist (p *fs-pairs*)
      (destructuring-bind (fn control n) p
        (dotimes (k 8)
          (dolist (args (%fs-arg-lists n))
            (let ((fast nil) (slow nil))
              (funcall (%fs-bindings k)
                       (lambda () (setq fast (%fs-run (symbol-function fn) args nil control))))
              (funcall (%fs-bindings k)
                       (lambda () (setq slow (%fs-run nil args t control))))
              (unless (equal fast slow)
                (push (list fn k args fast slow) diffs)))))))
    (length diffs))
  0)

(defun %fs-nil-dest (v) (format nil "<~A>" v))
(defun %fs-t-dest (v) (format t "<~A>~%" v))

(deftest format-simple-expansion.destination-nil-returns-string
  (list (%fs-nil-dest 5) (stringp (%fs-nil-dest 5)) (%fs-nil-dest "x") (format nil "n=~D" 1/2))
  ("<5>" t "<x>" "n=1/2"))

(deftest format-simple-expansion.destination-t-text
  (let ((s (make-string-output-stream)) (r nil))
    (let ((*standard-output* s)) (setq r (%fs-t-dest 7)))
    (list r (get-output-stream-string s)))
  (nil "<7>
"))

(deftest format-simple-expansion.fill-pointer-string-destination
  (let ((str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (list (format str "a~Db" 5) str))
  (nil "a5b"))

(deftest format-simple-expansion.other-destinations
  (let* ((s (make-string-output-stream))
         (b (make-broadcast-stream s))
         (tw (make-two-way-stream (make-string-input-stream "") s)))
    (format b "[~A]" 1)
    (format tw "[~S]" "q")
    (get-output-stream-string s))
  "[1][\"q\"]")

(defun %fs-bad-dest (d) (format d "~A" 1))

(deftest format-simple-expansion.bad-destination-signals-as-before
  (list (handler-case (%fs-bad-dest 5) (error () :error))
        (handler-case (apply #'format 5 "~A" '(1)) (error () :error)))
  (:error :error))

(defun %fs-missing () (format nil "~A ~A" 1))
(defun %fs-extra () (format nil "~A" 1 2 3))

(deftest format-simple-expansion.argument-count-as-before
  (list (eq (handler-case (%fs-missing) (error (e) (type-of e)))
            (handler-case (apply #'format nil "~A ~A" '(1)) (error (e) (type-of e))))
        (handler-case (%fs-missing) (error () :error))
        (%fs-extra))
  (t :error "1"))

(defun %fs-columns (s)
  (format s "ab~%")
  (fresh-line s)
  (format s "cd")
  (fresh-line s)
  (format s "~A~%" "ef"))

(deftest format-simple-expansion.newline-keeps-column-state
  (let ((s (make-string-output-stream)))
    (%fs-columns s)
    (get-output-stream-string s))
  "ab
cd
ef
")

(deftest format-simple-expansion.evaluation-order
  (let ((log '()))
    (format (progn (push :dest log) nil) "~A~A" (progn (push :a log) 1) (progn (push :b log) 2))
    (reverse log))
  (:dest :a :b))

(deftest format-simple-expansion.directive-case-and-other-directives-unchanged
  (list (format nil "~a|~s|~d" "x" "x" 12) (format nil "~5D|~@A" 3 "z") (format nil "~&a~&b"))
  ("x|\"x\"|12" "    3|z" "a
b"))
