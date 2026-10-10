;;; (LOAD-TIME-VALUE C) of a constant variable C is C's value: evaluating C at
;;; load time or where the form is gives the same object. It is now compiled as
;;; a read of C instead of a load-time-value slot (a lookup, a test and a store
;;; through full calls). ironclad's INLINE STREEBOG-AX holds its table as
;;; (LOAD-TIME-VALUE +STREEBOG-AX+ T), and STREEBOG-G expands it 640 times.

(setf dotcl:*save-sil* t)

(defconstant +ltc-table+
  (if (boundp '+ltc-table+) (symbol-value '+ltc-table+) (vector 10 20 30)))

(defun %ltc-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %ltc-read (i)
  (aref (load-time-value +ltc-table+ t) i))

(defun %ltc-same ()
  (eq (load-time-value +ltc-table+) +ltc-table+))

;; Emitting only: the emit-free interpreter has no LOAD-TIME-VALUE of its own
;; (the other load-time-value tests are gated the same way).
(deftest-emitting-only ltc-values
  (list (%ltc-read 0) (%ltc-read 2) (%ltc-same))
  (10 30 t))

(deftest-compiled-only ltc-no-slot
  (%ltc-sil-count #'%ltc-read "LTV-SLOT")
  0)
