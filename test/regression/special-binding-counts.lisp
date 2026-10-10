;;; A read of a special variable that is not bound on the current thread takes
;;; the global value without scanning the binding stack. Each thread keeps a
;;; count of its own bindings per symbol; these check that the count follows
;;; every way a binding ends, and that another thread's binding is not seen.

(require "dotcl-thread")

(defvar *sbc-x* 1)
(defun sbc-read () *sbc-x*)
(defun sbc-set (v) (setq *sbc-x* v))

(deftest special-binding-counts.let
  (list (sbc-read) (let ((*sbc-x* 2)) (sbc-read)) (sbc-read))
  (1 2 1))

(deftest special-binding-counts.nested-same-symbol
  (list (let ((*sbc-x* 2))
          (list (let ((*sbc-x* 3)) (sbc-read)) (sbc-read)))
        (sbc-read))
  ((3 2) 1))

(deftest special-binding-counts.after-throw
  (list (catch 'sbc (let ((*sbc-x* 3)) (throw 'sbc (sbc-read))))
        (sbc-read)
        (let ((*sbc-x* 4)) (sbc-read))
        (sbc-read))
  (3 1 4 1))

(deftest special-binding-counts.after-error
  (list (handler-case (let ((*sbc-x* 5)) (error "x")) (error () (sbc-read)))
        (sbc-read))
  (1 1))

(deftest special-binding-counts.setq-inside-and-outside
  (list (let ((*sbc-x* 6)) (sbc-set 7) (sbc-read))
        (sbc-read)
        (progn (sbc-set 8) (prog1 (sbc-read) (sbc-set 1))))
  (7 1 8))

(deftest special-binding-counts.progv-unbound
  (list (progv '(*sbc-x*) '() (boundp '*sbc-x*))
        (boundp '*sbc-x*)
        (progv '(*sbc-x*) '(9) (sbc-read))
        (sbc-read))
  (nil t 9 1))

(deftest special-binding-counts.makunbound-inside-binding
  (list (let ((*sbc-x* 2)) (makunbound '*sbc-x*) (boundp '*sbc-x*))
        (sbc-read))
  (nil 1))

;; A new thread does not inherit the creating thread's bindings, and a binding
;; made on another thread does not change what this thread reads.
(deftest special-binding-counts.other-thread
  (list (let ((*sbc-x* 10))
          (dotcl-thread:thread-join (dotcl-thread:make-thread #'sbc-read)))
        (let* ((ready nil) (done nil)
               (th (dotcl-thread:make-thread
                    (lambda ()
                      (let ((*sbc-x* 11))
                        (setq ready t)
                        (loop until done do (sleep 0.001))
                        (sbc-read))))))
          (loop until ready do (sleep 0.001))
          (prog1 (sbc-read)
            (setq done t)
            (dotcl-thread:thread-join th))))
  (1 1))
