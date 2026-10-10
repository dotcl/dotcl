;;; A call to a global macro in an integer operand position is looked at
;;; through its expansion, as a MACROLET-bound one already was. A library that
;;; names its word arithmetic with macros -- (defmacro p+ (a b)
;;; `(ldb (byte 62 0) (+ ,a ,b))) -- had every use compiled as a call the typed
;;; paths could not see into, so the store around it took the generic path and
;;; boxed (96 B an iteration here), where the same form written out was 0 B.

(defmacro %gmto-p+ (a b) `(ldb (byte 62 0) (+ ,a ,b)))
(defmacro %gmto-rot (x n) `(logior (ldb (byte 62 0) (ash ,x ,n)) (ash ,x ,(- n 62))))
;; A macro that expands into another macro call.
(defmacro %gmto-step (a b) `(%gmto-rot (%gmto-p+ ,a ,b) 7))

(defun %gmto-mix (w n)
  (declare (type (simple-array (unsigned-byte 62) (*)) w) (fixnum n))
  (dotimes (i n)
    (setf (aref w 0) (%gmto-p+ (aref w 0) (aref w 1))
          (aref w 1) (%gmto-step (aref w 1) (aref w 0))))
  (list (aref w 0) (aref w 1)))

(defun %gmto-hand (w n)
  (declare (type (simple-array (unsigned-byte 62) (*)) w) (fixnum n))
  (dotimes (i n)
    (setf (aref w 0) (ldb (byte 62 0) (+ (aref w 0) (aref w 1)))
          (aref w 1) (logior (ldb (byte 62 0)
                                  (ash (ldb (byte 62 0) (+ (aref w 1) (aref w 0))) 7))
                             (ash (ldb (byte 62 0) (+ (aref w 1) (aref w 0))) -55))))
  (list (aref w 0) (aref w 1)))

;; A local function of the macro's name is a call, not the macro.
(defun %gmto-shadow (w)
  (declare (type (simple-array (unsigned-byte 62) (*)) w))
  (flet ((%gmto-p+ (a b) (- a b)))
    (setf (aref w 0) (%gmto-p+ (aref w 0) (aref w 1)))
    (aref w 0)))

(defun %gmto-vec (&rest xs)
  (make-array (length xs) :element-type '(unsigned-byte 62) :initial-contents xs))

(deftest global-macro-typed-operand.values
  (list (equal (%gmto-mix (%gmto-vec 1 3) 1000) (%gmto-hand (%gmto-vec 1 3) 1000))
        (%gmto-mix (%gmto-vec (1- (ash 1 62)) 1) 1)
        (%gmto-shadow (%gmto-vec 10 3)))
  (t (0 128) 7))

(deftest-emitting-only global-macro-typed-operand.no-allocation
  (let ((w (%gmto-vec 1 3)))
    (%gmto-mix w 1000)
    (let ((best nil))
      (dotimes (k 3)
        (let ((b0 (nth 4 (dotcl:gc-stats))))
          (%gmto-mix w 10000)
          (let ((d (- (nth 4 (dotcl:gc-stats)) b0)))
            (setf best (if best (min best d) d)))))
      ;; The LIST of the result is the only allocation per call.
      (< best 4000)))
  t)
