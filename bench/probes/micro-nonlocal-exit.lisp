;;; bench/probes/micro-nonlocal-exit.lisp -- what one non-local exit costs
;;;
;;; Usage (from the project root, ALWAYS Release):
;;;   dotnet run -c Release --project runtime/runtime.csproj -- \
;;;     --asm compiler/cil-out.sil bench/probes/micro-nonlocal-exit.lisp
;;;   sbcl --script bench/probes/micro-nonlocal-exit.lisp
;;;
;;; Each row is ns per iteration (minimum of several rounds of at least 20 ms
;;; after a warm-up), loop overhead included. "exit" rows actually leave;
;;; "normal" rows set up the same construct and fall out of it normally.

(defvar *n* 20000)
(defvar *rounds* 7)

(defun now-ns ()
  (* (get-internal-real-time)
     (/ 1000000000 internal-time-units-per-second)))

(defmacro row (label n &body body)
  ;; N is grown until one round takes at least 20 ms, so a coarse clock or a
  ;; fast row still reads in ns.
  `(let ((best nil) (n ,n))
     (loop (let ((t0 (now-ns)))
             (dotimes (i n) ,@body)
             (when (>= (- (now-ns) t0) 20000000) (return))
             (setf n (* n 4))))
     (dotimes (r (+ 2 *rounds*))
       (let ((t0 (now-ns)))
         (dotimes (i n) ,@body)
         (let ((dt (/ (- (now-ns) t0) (float n))))
           (when (and (>= r 2) (or (null best) (< dt best)))
             (setf best dt)))))
     (format t "~40A ~10,1F ns~%" ,label best)))

(define-condition my-error (error) ())
(define-condition my-signal (condition) ())

(defvar *list* (list 1 2 3 4 5 6 7 8))
(defvar *strings* (list "12x" "abc" "" "-"))
(defvar *ht* (let ((h (make-hash-table)))
               (dotimes (i 8 h) (setf (gethash i h) i))))
(defvar *sink* nil)

(defun call-it (f) (funcall f))
(defun my-each (f list) (dolist (x list) (funcall f x)))
(defun raise-my-error () (error 'my-error))
(defun raise-signal () (signal 'my-signal) nil)
(defun throw-it (tag) (throw tag 1))

;; block / return-from from a lambda handed to a function
(defun rf-mapc (k)
  (block found (mapc (lambda (x) (when (eql x k) (return-from found x))) *list*) nil))
(defun rf-user-each (k)
  (block found (my-each (lambda (x) (when (eql x k) (return-from found x))) *list*) nil))
(defun rf-maphash (k)
  (block found (maphash (lambda (key v) (when (eql key k) (return-from found v))) *ht*) nil))
(defvar *vec* (coerce *list* 'vector))
(defun rf-map-nil (k)
  (block found (map nil (lambda (x) (when (eql x k) (return-from found x))) *vec*) nil))
(defun rf-mapcar (k)
  (block found (mapcar (lambda (x) (when (eql x k) (return-from found x))) *list*) nil))
(defun rf-hash-iterator (k)
  (with-hash-table-iterator (next *ht*)
    (loop (multiple-value-bind (more key v) (next)
            (unless more (return nil))
            (when (eql key k) (return v))))))
(defun rf-funcall-closure ()
  (block b (call-it (lambda () (return-from b 1))) 2))
(defun rf-labels (k)
  (block found
    (labels ((walk (l) (when l (when (eql (car l) k) (return-from found k)) (walk (cdr l)))))
      (walk *list*))
    nil))

;; catch / throw
(defun ct-callee () (catch 'tag (throw-it 'tag)))
(defun ct-closure () (catch 'tag (call-it (lambda () (throw 'tag 1)))))
(defun ct-normal () (catch 'tag (car *list*)))

;; handler-case / ignore-errors
(defun hc-parse (s) (handler-case (parse-integer s) (parse-error () nil)))
(defun ie-parse (s) (ignore-errors (parse-integer s)))
(defun hc-my-error () (handler-case (raise-my-error) (my-error () nil)))
(defun hc-error-inline () (handler-case (error 'my-error) (error () nil)))
(defun hc-error-string () (handler-case (error "x ~A" 1) (error () nil)))
(defun hc-signal () (handler-case (raise-signal) (my-signal () 1)))
(defun hc-normal (s) (handler-case (car s) (error () nil)))
(defun ie-normal (s) (ignore-errors (car s)))
(defun hb-decline () (handler-bind ((my-signal (lambda (c) c))) (raise-signal)))

;; restart-case
(defun rc-invoke ()
  (restart-case (invoke-restart 'skip) (skip () 1)))
(defun rc-from-handler ()
  (handler-bind ((my-error (lambda (c) (declare (ignore c)) (invoke-restart 'skip))))
    (restart-case (raise-my-error) (skip () 1))))
(defun rc-normal () (restart-case (car *list*) (skip () 1)))
(defun up-normal () (unwind-protect (car *list*) (setf *sink* nil)))

(row "baseline empty loop" 200000 nil)
(row "normal: catch" 200000 (ct-normal))
(row "normal: handler-case" 200000 (hc-normal *list*))
(row "normal: ignore-errors" 200000 (ie-normal *list*))
(row "normal: restart-case" 200000 (rc-normal))
(row "normal: unwind-protect" 200000 (up-normal))
(row "normal: signal, handler declines" 20000 (hb-decline))
(row "exit: labels return-from" *n* (rf-labels 4))
(row "exit: mapc lambda return-from" *n* (rf-mapc 4))
(row "exit: maphash lambda return-from" *n* (rf-maphash 4))
(row "exit: map nil lambda return-from" *n* (rf-map-nil 4))
(row "exit: mapcar lambda return-from" *n* (rf-mapcar 4))
(row "exit: hash-table-iterator return" *n* (rf-hash-iterator 4))
(row "exit: user fn lambda return-from" *n* (rf-user-each 4))
(row "exit: funcall closure return-from" *n* (rf-funcall-closure))
(row "exit: catch / throw from callee" *n* (ct-callee))
(row "exit: catch / throw from closure" *n* (ct-closure))
(row "exit: handler-case parse-integer" *n* (hc-parse "abc"))
(row "exit: ignore-errors parse-integer" *n* (ie-parse "abc"))
(row "exit: handler-case error (callee)" *n* (hc-my-error))
(row "exit: handler-case error (inline)" *n* (hc-error-inline))
(row "exit: handler-case error string" *n* (hc-error-string))
(row "exit: handler-case signal" *n* (hc-signal))
(row "exit: restart-case invoke-restart" *n* (rc-invoke))
(row "exit: restart from handler-bind" *n* (rc-from-handler))
