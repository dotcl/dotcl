;;; What GET-MACRO-CHARACTER hands back has to be the same object every time.
;;;
;;; A library that builds a readtable out of others has to decide whether two
;;; readtables agree on a character, and the only thing it can compare is the
;;; two reader macro functions. named-readtables compares them with EQ. The
;;; built-in macros have no Lisp function to hand back -- only a C# delegate --
;;; so one is wrapped around it, and wrapping on every call made the answer a
;;; different object each time. Every character with a built-in macro then
;;; looked like a disagreement, the standard " among them, and
;;; (defreadtable ... (:merge :standard)) could not run at all.
;;;
;;; The same question for a dispatching character has a second half: # is one
;;; dispatcher plus a table of sub-characters. The table belongs to the
;;; readtable, the dispatcher does not, so the dispatcher is shared and #
;;; compares equal across copies too.

(deftest readtable-macro-fn-same-readtable-twice
  (let ((rt (copy-readtable nil)))
    (eq (get-macro-character #\" rt) (get-macro-character #\" rt)))
  t)

(deftest readtable-macro-fn-two-copies
  (let ((a (copy-readtable nil))
        (b (copy-readtable nil)))
    (eq (get-macro-character #\" a) (get-macro-character #\" b)))
  t)

(deftest readtable-macro-fn-copy-vs-current
  (let ((a (copy-readtable nil)))
    (eq (get-macro-character #\" a) (get-macro-character #\" nil)))
  t)

(deftest readtable-macro-fn-every-standard-macro-char
  (let ((a (copy-readtable nil))
        (b (copy-readtable nil)))
    (loop for ch across "\"'(),;`#"
          always (eq (get-macro-character ch a) (get-macro-character ch b))))
  t)

(deftest readtable-dispatch-fn-two-copies
  (let ((a (copy-readtable nil))
        (b (copy-readtable nil)))
    (eq (get-macro-character #\# a) (get-macro-character #\# b)))
  t)

(deftest readtable-dispatch-sub-fn-two-copies
  (let ((a (copy-readtable nil))
        (b (copy-readtable nil)))
    (loop for ch across "\\'(*"
          always (eq (get-dispatch-macro-character #\# ch a)
                     (get-dispatch-macro-character #\# ch b))))
  t)

;;; The identity must not be bought by losing the answer: a function set by the
;;; user comes back as itself, and a sub-character set in one readtable does not
;;; appear in another.

(deftest readtable-user-macro-fn-is-returned-as-is
  (let ((rt (copy-readtable nil))
        (fn (lambda (stream char) (declare (ignore stream char)) :mine)))
    (set-macro-character #\~ fn nil rt)
    (eq (get-macro-character #\~ rt) fn))
  t)

(deftest readtable-user-dispatch-fn-does-not-leak-to-a-copy
  (let ((a (copy-readtable nil))
        (b (copy-readtable nil))
        (fn (lambda (stream char n) (declare (ignore stream char n)) :mine)))
    (set-dispatch-macro-character #\# #\~ fn a)
    (list (eq (get-dispatch-macro-character #\# #\~ a) fn)
          (get-dispatch-macro-character #\# #\~ b)))
  (t nil))

;;; And the shared dispatcher still dispatches in the readtable in force, not in
;;; whichever one it was fetched from.

(deftest readtable-dispatch-reads-in-the-readtable-in-force
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\~ (lambda (s c n)
                                            (declare (ignore s c n))
                                            :from-rt)
                                  rt)
    (list (let ((*readtable* rt)) (read-from-string "#~"))
          (let ((*readtable* (copy-readtable nil)))
            (handler-case (read-from-string "#~")
              (error () :signalled)))))
  (:from-rt :signalled))
