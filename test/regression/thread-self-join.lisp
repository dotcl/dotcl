;;; Joining the current thread signals an error instead of waiting for ever,
;;; on a thread MAKE-THREAD made and on one it did not (the main thread here).

(deftest thread-self-join.main-thread
  (handler-case (progn (dotcl:thread-join (dotcl:current-thread)) :returned)
    (error () :error))
  :error)

(deftest thread-self-join.lisp-made-thread
  (dotcl:thread-join
   (dotcl:make-thread
    (lambda ()
      (handler-case (progn (dotcl:thread-join (dotcl:current-thread)) :returned)
        (error () :error)))))
  :error)
