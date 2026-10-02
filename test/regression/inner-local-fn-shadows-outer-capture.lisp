;;; A call to a local function inside an inner FLET / LABELS of the same name
;;; refers to the inner binding. The free-variable analysis did not count the
;;; inner binding as bound for the lambda being analyzed, so a closure that
;;; contained the inner LABELS (here the body of a HANDLER-CASE that also binds
;;; specials with PROGV) was taken to capture the OUTER function of that name.
;;; When the outer FLET function had no local of its own the compile failed
;;; with "Undeclared local: XG_n". Found by the random integer form test
;;; (make test-random-forms RANDOM_EXTRA=1).

(defvar *iflsoc-1*)
(defvar *iflsoc-2*)
(defvar *iflsoc-3*)

;; COMPILE of the lambda the random form test reported, as it reported it.
(defun %iflsoc-random-form ()
  (funcall
   (compile nil
            '(lambda (a b c d) (declare (ignorable a b c d))
               (flet ((xg (xp &optional (xq (unwind-protect 1 1 1))) (+ xp xq)))
                 (xg (let ((v2 (handler-case
                                   (truncate (labels ((xg (xn xacc)
                                                        (if (<= xn 0)
                                                            xacc
                                                            (xg (1- xn) (+ xacc 2251799813685256)))))
                                               (xg 3 4))
                                             (- (progv '(*iflsoc-1* *iflsoc-2* *iflsoc-3*) (list 1 2 3) 7)
                                                (progv '(*iflsoc-1* *iflsoc-2* *iflsoc-3*) (list 1 2 3) 7)))
                                 (division-by-zero () 1))))
                       0)))))
   1 2 3 4))

(defun %iflsoc-closure (n)
  (flet ((xg (x) (list :outer x)))
    (xg (funcall (lambda ()
                   (labels ((xg (k acc) (if (<= k 0) acc (xg (1- k) (+ acc 1)))))
                     (xg n 0)))))))

(deftest-emitting-only inner-local-fn-shadows-outer-capture-compile
  (%iflsoc-random-form)
  1)

(deftest inner-local-fn-shadows-outer-capture
  (%iflsoc-closure 3)
  (:outer 3))
