;;; Deciding whether an integer expression can be computed in raw int64 asks,
;;; for each operand of a LOGAND, both "is it a fixnum" and "what is its range",
;;; and each of those asks the same of the operand's own operands. A long LOGAND
;;; is folded into a left-associated chain of two-operand calls, so the work
;;; doubled with each operand: a 30-operand LOGAND of a special variable bound
;;; by LET (PROG1 expands into one) took minutes to compile, and 34 operands
;;; hung the random integer form test (make test-random-forms).

(defvar *lcct-s* -130)

(defun %lcct-prog1 ()
  (prog1 (logand 61652511629 *lcct-s* 238 *lcct-s* *lcct-s* 1048577 *lcct-s*
                 *lcct-s* *lcct-s* -45357271 *lcct-s* -56608423 68719476734
                 *lcct-s* -273957 790286343623 *lcct-s* *lcct-s* *lcct-s* 3486043
                 -32379 -2700028054 7 *lcct-s* *lcct-s* *lcct-s* 498844164960619591
                 -21602594 93 *lcct-s* 140737488355329 *lcct-s*)))

(defun %lcct-let (x)
  (let ((v (logior x 1 x 2 x 4 x 8 x 16 x 32 x 64 x 128 x 256 x 512 x 1024 x
                   2048 x 4096 x 8192 x 16384 x 32768 x)))
    v))

(deftest logand-chain-compile-time
  (list (%lcct-prog1) (%lcct-let 0) (%lcct-let -65536))
  (0 65535 -1))
