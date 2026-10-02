;;; An argument is one value, the primary one, even when the call around it is
;;; in tail position or inside MULTIPLE-VALUE-BIND. GETHASH and SUBSEQ compiled
;;; their arguments in the enclosing context, so a table or sequence coming
;;; from a function that returns two values reached the runtime still wrapped:
;;; "GETHASH: not a hash-table". reblocks' MAKE-ACTION calls
;;; (gethash fn (get-value 'action->code (make-weak-hash-table ...))) where
;;; GET-VALUE returns ENSURE-GETHASH's two values. One-argument APPEND and
;;; LIST* returned all the values of their argument instead of one.

(defvar *svaic-table* (make-hash-table))
(setf (gethash 1 *svaic-table*) :one)
(defun svaic-table () (values *svaic-table* t))
(defun svaic-list () (values (list 1 2 3) t))
(defun svaic-string () (values "abcd" t))
(defun svaic-index () (values 1 2))

(defun svaic-gethash-tail () (gethash 1 (svaic-table)))
(defun svaic-gethash-mvb ()
  (multiple-value-bind (v found) (gethash 1 (svaic-table) :none) (list v found)))
(defun svaic-subseq-tail () (subseq (svaic-string) (svaic-index)))
(defun svaic-append-1 () (append (svaic-list)))
(defun svaic-list*-1 () (list* (svaic-list)))

(deftest single-valued-args-of-inlined-calls
  (list (multiple-value-list (svaic-gethash-tail))
        (svaic-gethash-mvb)
        (multiple-value-list (svaic-subseq-tail))
        (multiple-value-list (svaic-append-1))
        (multiple-value-list (svaic-list*-1))
        (multiple-value-list (gethash 1 (svaic-table))))
  ((:one t) (:one t) ("bcd") ((1 2 3)) ((1 2 3)) (:one t)))
