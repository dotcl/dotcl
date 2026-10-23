;;; A MACROLET binding is lexical: it is not visible to another thread, nor
;;; outside the MACROLET form. The compiler used to put the local expander in
;;; the global macro table while it compiled the body, so a second thread
;;; compiling a MACROLET of the same name replaced it, and the first thread
;;; expanded its call with the other thread's expander. Here the first thread
;;; holds inside its MACROLET body until the second thread is inside its own and
;;; the main thread has looked at the global definition.

(require "dotcl-thread")

(defvar *mlt-a-in* nil)
(defvar *mlt-b-in* nil)
(defvar *mlt-a-done* nil)
(defvar *mlt-checked* nil)
(defvar *mlt-global-seen* :unset)

(defun %mlt-wait (var)
  (loop repeat 2000 until (symbol-value var) do (sleep 0.005))
  (symbol-value var))

(defmacro %mlt-a-hold ()
  (setf *mlt-a-in* t)
  (%mlt-wait '*mlt-checked*)
  nil)

(defmacro %mlt-b-hold ()
  (setf *mlt-b-in* t)
  (%mlt-wait '*mlt-a-done*)
  nil)

(deftest-compiled-only macrolet-threads.local-to-its-thread
  (let ((a nil) (b nil))
    (setf *mlt-a-in* nil *mlt-b-in* nil *mlt-a-done* nil *mlt-checked* nil
          *mlt-global-seen* :unset)
    (let ((ta (dotcl-thread:make-thread
               (lambda ()
                 (setf a (funcall (compile nil '(lambda ()
                                                 (macrolet ((%mlt-m () 1))
                                                   (%mlt-a-hold)
                                                   (%mlt-m))))))
                 (setf *mlt-a-done* t))))
          (tb nil))
      (%mlt-wait '*mlt-a-in*)
      (setf tb (dotcl-thread:make-thread
                (lambda ()
                  (setf b (funcall (compile nil '(lambda ()
                                                  (macrolet ((%mlt-m () 2))
                                                    (%mlt-b-hold)
                                                    (%mlt-m)))))))))
      (%mlt-wait '*mlt-b-in*)
      (setf *mlt-global-seen* (and (macro-function '%mlt-m) t)
            *mlt-checked* t)
      (dotcl-thread:thread-join ta)
      (dotcl-thread:thread-join tb))
    (list a b *mlt-global-seen* (and (macro-function '%mlt-m) t)))
  (1 2 nil nil))
