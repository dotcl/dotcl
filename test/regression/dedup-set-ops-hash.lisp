;;; REMOVE-DUPLICATES / DELETE-DUPLICATES and the set operations look elements up in a hash
;;; table for EQL, EQUAL and EQUALP (as SBCL does): 100000 elements were a minute or more of
;;; pairwise comparison. The results are the ones the pairwise definition gives, in the same
;;; order, for :from-end, :start / :end, :key, lists and vectors.

(defun %dsh-ref-remove-duplicates (seq test &key from-end (start 0) end (key #'identity))
  "The pairwise definition (CLHS), on a list."
  (let* ((v (coerce seq 'vector)) (end (or end (length v))) (out '()))
    (dotimes (i (length v))
      (let ((x (aref v i)))
        (if (or (< i start) (>= i end))
            (push x out)
            (let ((dup (if from-end
                           (loop for j from start below i
                                 thereis (funcall test (funcall key (aref v j)) (funcall key x)))
                           (loop for j from (1+ i) below end
                                 thereis (funcall test (funcall key x) (funcall key (aref v j)))))))
              (unless dup (push x out))))))
    (nreverse out)))

(defvar *dsh-data*
  (loop for i below 120 collect (format nil "k~d" (mod (* i 7) 37))))
(defvar *dsh-nums*
  (loop for i below 120 collect (mod (* i 11) 41)))
(defvar *dsh-mixed*
  (loop for i below 100 collect (if (evenp i) (string-upcase (format nil "a~d" (mod i 9))) (format nil "A~d" (mod i 9)))))

(deftest dedup-set-ops-hash.remove-duplicates-matches-reference
  (let ((bad '()))
    (dolist (case (list (list *dsh-data* #'equal) (list *dsh-nums* #'eql) (list *dsh-mixed* #'equalp)
                        (list *dsh-mixed* #'equal)))
      (destructuring-bind (data test) case
        (dolist (from-end '(nil t))
          (dolist (range '((0 nil) (5 90) (0 30) (60 nil)))
            (let ((got (remove-duplicates data :test test :from-end from-end
                                               :start (first range) :end (second range)))
                  (want (%dsh-ref-remove-duplicates data test :from-end from-end
                                                    :start (first range) :end (second range))))
              (unless (equal got want) (push (list test from-end range) bad)))))))
    bad)
  nil)

(deftest dedup-set-ops-hash.default-key-and-vectors
  (list (equal (remove-duplicates *dsh-nums*) (%dsh-ref-remove-duplicates *dsh-nums* #'eql))
        (equal (remove-duplicates *dsh-data* :test #'equal :key (lambda (s) (subseq s 0 2)))
               (%dsh-ref-remove-duplicates *dsh-data* #'equal :key (lambda (s) (subseq s 0 2))))
        (equalp (remove-duplicates (coerce *dsh-nums* 'vector))
                (coerce (%dsh-ref-remove-duplicates *dsh-nums* #'eql) 'vector))
        (equal (remove-duplicates (coerce (loop for i below 100 collect (code-char (+ 97 (mod i 13)))) 'string))
               (coerce (%dsh-ref-remove-duplicates (loop for i below 100 collect (code-char (+ 97 (mod i 13)))) #'eql) 'string))
        (equal (remove-duplicates *dsh-nums* :test-not #'/=)
               (%dsh-ref-remove-duplicates *dsh-nums* #'=))
        (equal (delete-duplicates (copy-list *dsh-data*) :test #'equal :from-end t)
               (%dsh-ref-remove-duplicates *dsh-data* #'equal :from-end t)))
  (t t t t t t))

(deftest dedup-set-ops-hash.set-operations-match-membership
  (let* ((a (loop for i below 80 collect (mod (* i 5) 43)))
         (b (loop for i below 70 collect (mod (* i 3) 31)))
         (sa (loop for x in a collect (format nil "s~d" x)))
         (sb (loop for x in b collect (format nil "s~d" x)))
         (naive-int (remove-if-not (lambda (x) (member x b)) a))
         (naive-diff (remove-if (lambda (x) (member x b)) a)))
    (list (equal (intersection a b) naive-int)
          (equal (set-difference a b) naive-diff)
          (equal (sort (remove-duplicates (union a b)) #'<) (sort (remove-duplicates (append a b)) #'<))
          (subsetp naive-int b) (subsetp a b)
          (equal (intersection sa sb :test #'equal) (remove-if-not (lambda (x) (member x sb :test #'equal)) sa))
          (equal (set-difference sa sb :test 'equal) (remove-if (lambda (x) (member x sb :test #'equal)) sa))
          (equal (intersection sa sb :key #'length :test #'eql)
                 (remove-if-not (lambda (x) (member (length x) sb :key #'length)) sa))
          (equal (sort (copy-list (set-exclusive-or a b)) #'<)
                 (sort (append (remove-if (lambda (x) (member x b)) a) (remove-if (lambda (x) (member x a)) b)) #'<))
          (equal (intersection sa (mapcar #'string-upcase sb) :test #'equalp)
                 (remove-if-not (lambda (x) (member x sb :test #'equalp)) sa))))
  (t t t t nil t t t t t))

(deftest dedup-set-ops-hash.large-input-is-fast
  (let* ((n 100000)
         (a (loop for i below n collect (format nil "k~d" (mod i 50000))))
         (b (loop for i below n collect (format nil "k~d" (mod (+ i 25000) 50000))))
         (start (get-internal-real-time))
         (u (remove-duplicates (merge 'list (sort (copy-list a) #'string<) (sort (copy-list b) #'string<) #'string<)
                               :test #'equal))
         (i (intersection a b :test #'equal))
         (d (set-difference a (subseq b 0 20000) :test #'equal))
         (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (list (length u) (> (length i) 0) (> (length d) 0) (< elapsed 4)))
  (50000 t t t))

;; A dotted LIST2 only fails when the walk reaches its tail: the lookup must not look
;; further than the walk does.
(deftest dedup-set-ops-hash.dotted-second-argument-not-reached
  (let ((l2 (append (loop for i below 30 collect i) 99)))
    (list (intersection '(1 2) (append (loop for i below 30 collect i) 99))
          (handler-case (intersection '(1000) l2) (error () :error))
          (handler-case (set-difference '(5) l2) (error () :error))))
  ((1 2) :error nil))
