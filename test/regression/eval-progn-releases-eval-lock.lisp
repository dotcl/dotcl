;;; EVAL of a top level PROGN evaluates its subforms one at a time. The
;;; subforms used to run while the PROGN's own hold on the process-wide eval
;;; lock was still taken, so another thread calling EVAL waited until the whole
;;; PROGN returned. SLIME starts its server from one PROGN at the REPL, and its
;;; worker threads then never answered. Here the PROGN waits (bounded) for
;;; another thread's EVAL to finish, which it can only do if the lock is free.

(require "dotcl-thread")

(defvar *eprl-done* nil)

(deftest eval-progn-lets-other-threads-eval
  (progn
    (setq *eprl-done* nil)
    (let ((result
            (eval '(progn
                    (let ((th (dotcl-thread:make-thread
                               (lambda ()
                                 (setq *eprl-done* (eval '(+ 1 2))))
                               :name "eprl-worker")))
                      (loop repeat 200
                            until *eprl-done*
                            do (sleep 0.05))
                      (list th *eprl-done*))))))
      ;; Join after the PROGN has returned, so a failing run still finishes.
      (dotcl-thread:thread-join (first result))
      (second result)))
  3)
