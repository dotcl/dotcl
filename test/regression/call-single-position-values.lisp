;;; A call in a single-value position (its result is unwrapped at once) is not
;;; preceded by an emptying of the values channel. Whatever the callee
;;; published must still not reach a reader of the enclosing function's values
;;; when that function passes the call's value out by a way other than a tail call.

(defvar *csp-h* (make-hash-table))
(defun %csp-nil7 () (values nil 7))
(defun %csp-nil3 () (values nil 1 2))
(defun %csp-bare-nil () nil)
(defun %csp-gh () (gethash 1 *csp-h*))

(defun %csp-or () (or (%csp-nil7) (%csp-nil3)))
(defun %csp-or2 () (or (%csp-gh) (%csp-bare-nil)))
(defun %csp-and () (and 1 (%csp-nil7) (%csp-bare-nil)))
(defun %csp-cond () (cond ((%csp-nil7)) ((%csp-nil3)) (t (%csp-bare-nil))))
(defun %csp-block () (block b (when (%csp-nil7) (return-from b 1)) (return-from b (%csp-gh))))
(defun %csp-prog1 () (prog1 (%csp-nil7) (%csp-nil3)))
(defun %csp-let () (let ((v (%csp-nil7))) (%csp-nil3) v))
(defun %csp-let2 () (let* ((v (%csp-nil7)) (w (%csp-gh))) (if v v w)))
(defun %csp-if () (if (%csp-nil7) 1 (%csp-gh)))
(defun %csp-catch () (catch 'csp (%csp-nil7) (%csp-gh)))
(defun %csp-hc () (handler-case (progn (%csp-nil3) (%csp-gh)) (error () 1)))
(defun %csp-up () (unwind-protect (progn (%csp-nil7) (%csp-gh)) (%csp-nil3)))
(defun %csp-case () (case (%csp-nil7) ((nil) (%csp-gh)) (t 1)))
(defun %csp-the () (the t (progn (%csp-nil7) (%csp-gh))))
(defun %csp-arg () (list (%csp-nil7) (%csp-nil3)))
(defun %csp-car () (car (list (%csp-nil7))))
(defun %csp-tagbody () (tagbody (%csp-nil7) (go done) done) (%csp-gh))
(defun %csp-loop (n) (let ((r nil)) (dotimes (i n) (setq r (%csp-nil7))) r))
(defun %csp-mvb () (multiple-value-bind (a b) (%csp-nil7) (list a b (%csp-gh))))
(defun %csp-mvl () (multiple-value-list (progn (%csp-nil7) (%csp-gh))))

(deftest call-single-position-values.nil
  (mapcar (lambda (f) (multiple-value-list (funcall f)))
          (list #'%csp-or #'%csp-or2 #'%csp-and #'%csp-cond #'%csp-block #'%csp-prog1
                #'%csp-let #'%csp-let2 #'%csp-if #'%csp-catch #'%csp-hc #'%csp-up
                #'%csp-case #'%csp-the #'%csp-arg #'%csp-car #'%csp-tagbody))
  ((nil 1 2) (nil) (nil) (nil) (nil nil) (nil) (nil) (nil) (nil nil)
   (nil nil) (nil nil) (nil nil) (nil nil) (nil nil) ((nil nil)) (nil) (nil nil)))

(deftest call-single-position-values.other
  (list (multiple-value-list (%csp-loop 3))
        (multiple-value-list (%csp-mvb))
        (multiple-value-list (%csp-mvl)))
  ((nil) ((nil 7 nil)) ((nil nil))))

;;; A multiple-value consumer that follows a call received in a single-value position
;;; must see only its own call's values.
(defun %csp-v0 () (values))
(defun %csp-v3 () (values 1 2 3))
(defun %csp-ident (x) x)

(deftest call-single-position-values.consumers
  (list
   ;; after a single-position receipt of a multiple-value call
   (progn (%csp-nil7) (multiple-value-list (%csp-nil3)))
   (progn (%csp-nil7) (multiple-value-list (%csp-gh)))
   (progn (%csp-nil7) (multiple-value-list (%csp-bare-nil)))
   (progn (%csp-nil3) (length (multiple-value-list (%csp-v0))))
   (progn (%csp-nil7) (multiple-value-list (values)))
   (list (%csp-nil7) (multiple-value-list (%csp-v3)))
   (multiple-value-bind (a b c) (progn (%csp-nil3) (%csp-bare-nil)) (list a b c))
   (multiple-value-bind (a b) (progn (%csp-nil7) (%csp-gh)) (list a b))
   (multiple-value-call #'list (%csp-nil7) (%csp-gh) (%csp-bare-nil))
   (multiple-value-list (multiple-value-prog1 (values 1 2) (%csp-nil3)))
   (multiple-value-list (prog1 (%csp-nil7) (%csp-nil3)))
   ;; funcall / apply
   (progn (%csp-nil7) (multiple-value-list (funcall #'%csp-bare-nil)))
   (progn (%csp-nil7) (multiple-value-list (apply #'%csp-gh nil)))
   (progn (%csp-nil3) (multiple-value-list (funcall '%csp-ident nil)))
   (list (%csp-nil7) (multiple-value-list (funcall #'%csp-nil3))
         (multiple-value-list (apply #'%csp-nil7 nil)))
   ;; across catch / throw / unwind-protect
   (multiple-value-list (catch 'csp (%csp-nil7) (throw 'csp (values 1 2))))
   (multiple-value-list (catch 'csp (%csp-nil7) (throw 'csp (%csp-bare-nil))))
   (progn (%csp-nil7) (multiple-value-list (catch 'csp (throw 'csp (values 1 2 3)))))
   (multiple-value-list (unwind-protect (values 1 2) (%csp-nil7)))
   (multiple-value-list (unwind-protect (%csp-bare-nil) (%csp-nil3)))
   (multiple-value-list (block b (unwind-protect (return-from b (values 4 5)) (%csp-nil7))))
   (multiple-value-list (handler-case (progn (%csp-nil7) (error "x")) (error () (%csp-bare-nil))))
   (multiple-value-list (handler-case (%csp-nil7) (error () 1)))
   (multiple-value-list (handler-bind ((error (lambda (c) (declare (ignore c)) (%csp-nil7))))
                          (progn (%csp-nil3) (%csp-bare-nil)))))
  ((nil 1 2) (nil nil) (nil) 0 nil (nil (1 2 3)) (nil nil nil) (nil nil)
   (nil 7 nil nil nil) (1 2) (nil) (nil) (nil nil) (nil) (nil (nil 1 2) (nil 7))
   (1 2) (nil) (1 2 3) (1 2) (nil) (4 5) (nil) (nil 7) (nil)))

(defun %csp-thread-body ()
  (let ((r '()))
    (dotimes (i 200)
      (%csp-nil7)
      (push (multiple-value-list (%csp-nil3)) r)
      (push (multiple-value-list (%csp-bare-nil)) r))
    (every (lambda (x) (or (equal x '(nil 1 2)) (equal x '(nil)))) r)))

(require "dotcl-thread")

(deftest call-single-position-values.threads
  (let ((ths (loop repeat 4 collect (dotcl-thread:make-thread #'%csp-thread-body))))
    (mapcar #'dotcl-thread:thread-join ths))
  (t t t t))
