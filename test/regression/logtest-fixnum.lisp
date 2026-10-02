;;; LOGTEST went through BigInteger for every pair, fixnums included, and was
;;; called through the symbol's function. Two fixnums are now one AND, and the
;;; call is direct; so is LOGCOUNT's. Coalton's HashMap tests a trie bitmap with
;;; LOGTEST and counts its bits with LOGCOUNT 32 times per node it copies.

(defun %ltf-test (a b) (logtest a b))

(deftest logtest-fixnum.values
  (list (%ltf-test 6 1) (%ltf-test 6 2) (%ltf-test -1 0) (%ltf-test -1 -1)
        (%ltf-test most-negative-fixnum -1) (%ltf-test #x55555555 #xAAAAAAAA)
        (%ltf-test (expt 2 70) (expt 2 70)) (%ltf-test (expt 2 70) 1)
        (%ltf-test -1 (expt 2 70)) (%ltf-test (- (expt 2 70)) 1)
        (funcall 'logtest 3 1))
  (nil t nil t t nil t nil t nil t))

(deftest logtest-fixnum.type-error
  (handler-case (%ltf-test 1 1.0) (type-error () :type-error))
  :type-error)

(defun %ltf-bytes () (nth 4 (dotcl:gc-stats)))
(defvar *ltf-mask* #x5555555555)

(deftest-compiled-only logtest-fixnum.no-allocation
  (let ((m *ltf-mask*) (n 0))
    (dotimes (k 100) (%ltf-test m 4))
    (let ((b0 (%ltf-bytes)))
      (dotimes (k 100000) (when (%ltf-test m 4) (setq n (logand (1+ n) 1023))))
      (< (/ (- (%ltf-bytes) b0) 100000) 8)))
  t)

;; The compiled direct call and the function object agree.
(defun %ltf-count (x) (logcount x))
(deftest logtest-fixnum.logcount-direct
  (let ((xs (list 0 -1 #x5555555555 (- (expt 2 70)) (1- (expt 2 70)))))
    (equal (mapcar #'%ltf-count xs) (mapcar (symbol-function 'logcount) xs)))
  t)
