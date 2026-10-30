;;; Argument checks found by the sequence differential test against SBCL.

(deftest sequence-if-rejects-test-keywords
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (program-error () :program-error)))
          (list (lambda () (find-if #'evenp (list 1 2) :test #'=))
                (lambda () (find-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (position-if #'evenp (list 1 2) :test #'=))
                (lambda () (position-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (count-if #'evenp (list 1 2) :test #'=))
                (lambda () (count-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (remove-if #'evenp (list 1 2) :test #'=))
                (lambda () (remove-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (delete-if #'evenp (list 1 2) :test #'=))
                (lambda () (delete-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (substitute-if 9 #'evenp (list 1 2) :test #'=))
                (lambda () (substitute-if-not 9 #'evenp (list 1 2) :test-not #'=))
                (lambda () (nsubstitute-if 9 #'evenp (list 1 2) :test #'=))
                (lambda () (nsubstitute-if-not 9 #'evenp (list 1 2) :test-not #'=))
                (lambda () (member-if #'evenp (list 1 2) :test #'=))
                (lambda () (member-if-not #'evenp (list 1 2) :test-not #'=))
                (lambda () (assoc-if #'evenp (list (cons 1 2)) :test #'=))
                (lambda () (assoc-if-not #'evenp (list (cons 1 2)) :test-not #'=))
                (lambda () (rassoc-if #'evenp (list (cons 2 1)) :test #'=))
                (lambda () (rassoc-if-not #'evenp (list (cons 2 1)) :test-not #'=))))
  (:program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error))

(deftest sequence-if-allows-test-keyword-with-allow-other-keys
  (find-if #'evenp (list 1 2 3) :test #'= :allow-other-keys t)
  2)

(deftest sequence-string-case-checks-bounds
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (string-upcase "abc" :start 4))
                (lambda () (string-downcase "abc" :start 4 :end 2))
                (lambda () (string-capitalize "" :start 1))
                (lambda () (nstring-upcase (copy-seq "abc") :start 4))
                (lambda () (nstring-downcase (copy-seq "abc") :start 4 :end 2))
                (lambda () (nstring-capitalize (copy-seq "") :start 1))))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest sequence-nsubstitute-checks-list-bounds
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (nsubstitute 9 2 (list 1 2 3) :start -1))
                (lambda () (nsubstitute 9 2 (list 1 2 3) :end -1))
                (lambda () (nsubstitute 9 2 (list 1 2 3) :start 3 :end 1))
                (lambda () (nsubstitute-if 9 #'evenp (list 1 2 3) :start 99))
                (lambda () (nsubstitute-if-not 9 #'evenp (list 1 2 3) :end 99))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest sequence-nsubstitute-list-still-works-in-range
  (nsubstitute 9 2 (list 1 2 3) :start 1 :end 2)
  (1 9 3))

(deftest sequence-substitute-checks-empty-list-bounds
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (substitute 9 2 nil :start 1))
                (lambda () (nsubstitute 9 2 nil :end 1))))
  (:type-error :type-error))

(deftest sequence-substitute-family-rejects-improper-lists
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (substitute 9 2 (cons 1 2)))
                (lambda () (nsubstitute 9 2 (cons 1 2)))
                (lambda () (substitute-if 9 #'evenp (cons 1 2)))
                (lambda () (substitute-if-not 9 #'evenp (cons 1 2)))
                (lambda () (nsubstitute-if 9 #'evenp (cons 1 2)))
                (lambda () (nsubstitute-if-not 9 #'evenp (cons 1 2)))))
  (:type-error :type-error :type-error :type-error :type-error :type-error))

(deftest sequence-search-mismatch-concatenate-reject-improper-lists
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (search (cons 1 2) (list 1 2 3)))
                (lambda () (search (list 1 2) (cons 1 2)))
                (lambda () (mismatch (cons 1 2) (list 1 2 3)))
                (lambda () (mismatch (list 1 2) (cons 1 2)))
                (lambda () (concatenate 'list (list 1) (cons 2 3)))
                (lambda () (concatenate 'vector (list 1) (cons 2 3)))
                (lambda () (replace (list 1 2 3) (cons 2 3)))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest sequence-setf-subseq-copies-list-replacement
  (list (let ((v (vector 1 2 3 4 5)))
          (setf (subseq v 1 3) (list 9 8 7))
          (coerce v 'list))
        (let ((s (copy-seq "abcde")))
          (setf (subseq s 1 3) (list #\x #\y))
          s)
        (let ((b (make-array 5 :element-type 'bit :initial-contents '(1 0 1 0 1))))
          (setf (subseq b 1 3) (list 1 1 0))
          b))
  ((1 9 8 4 5) "axyde" #*11101))

(deftest sequence-setf-subseq-checks-string-element-type
  (handler-case
      (let ((s (copy-seq "abcde")))
        (setf (subseq s 1 3) (list 1 2))
        :no-error)
    (type-error () :type-error))
  :type-error)

(deftest sequence-map-requires-a-sequence
  (handler-case (map 'list #'list)
    (program-error () :program-error))
  :program-error)

;;; A bounding index is invalid even when the answer would not have read it:
;;; an empty sequence, or :COUNT 0. REMOVE, DELETE and the SUBSTITUTE family
;;; returned before checking those, so (remove 9 nil :end 1) and
;;; (substitute 9 1 '(1 2) :end 3 :count 0) answered where every other path
;;; signalled a type-error.
(deftest sequence-bounds-empty-list
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (remove 9 nil :end 1))
                (lambda () (delete 9 nil :start 1))
                (lambda () (remove-if #'evenp nil :end 1))
                (lambda () (remove-if-not #'evenp nil :end 1))
                (lambda () (delete-if #'evenp nil :end 1))
                (lambda () (delete-if-not #'evenp nil :end 1))
                (lambda () (remove 9 nil :end 1 :count 0))
                (lambda () (delete 9 nil :start 1 :count 0))))
  (:type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error))

(deftest sequence-bounds-count-zero
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (remove 9 '(1 2) :end 3 :count 0))
                (lambda () (delete 9 #(1 2) :start 3 :count 0))
                (lambda () (remove 9 "ab" :end 3 :count 0))
                (lambda () (delete-if #'evenp '(1 2) :end 3 :count 0))
                (lambda () (substitute 9 1 '(1 2) :end 3 :count 0))
                (lambda () (nsubstitute 9 1 #(1 2) :start 3 :count 0))
                (lambda () (substitute 9 1 "ab" :end 3 :count 0))
                (lambda () (nsubstitute 9 1 '(1 2) :end 3 :count 0))
                (lambda () (substitute-if 9 #'evenp '(1 2) :end 3 :count 0))
                (lambda () (nsubstitute-if 9 #'evenp '(1 2) :end 3 :count 0))))
  (:type-error :type-error :type-error :type-error :type-error
   :type-error :type-error :type-error :type-error :type-error))

(deftest sequence-bounds-remove-duplicates-empty-list
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (remove-duplicates nil :end 1))
                (lambda () (delete-duplicates nil :start 1))
                (lambda () (remove-duplicates nil :end 1 :from-end t))))
  (:type-error :type-error :type-error))

(deftest sequence-bounds-legal-edges
  (list (remove 9 nil)
        (remove 9 '(1 2) :end 2 :count 0)
        (substitute 9 1 '(1 2) :end 2 :count 0)
        (remove-duplicates nil))
  (nil (1 2) (1 2) nil))

;;; The same forms through EVAL on both evaluator paths: compiled and
;;; interpreted must signal the same condition, not one value and one error.
(defun %seq-bounds-outcome (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval form) :value)
      (type-error () :type-error)
      (error (e) (list :other (type-of e))))))

(deftest sequence-bounds-both-evaluators
  (let ((results
          (mapcar (lambda (form)
                    (list (%seq-bounds-outcome :compile form)
                          (%seq-bounds-outcome :interpret form)))
                  '((find 1 '(1) :end 2)
                    (find-if #'evenp '(1) :end 2)
                    (find-if-not #'evenp '(1) :end 2)
                    (position 1 '(1) :end 2)
                    (position-if #'evenp '(1) :end 2)
                    (position-if-not #'evenp '(1) :end 2)
                    (count 1 '(1) :end 2)
                    (count-if #'evenp '(1) :end 2)
                    (count-if-not #'evenp '(1) :end 2)
                    (remove 1 '(1) :end 2)
                    (delete 1 '(1) :end 2)
                    (remove-if #'evenp '(1) :end 2)
                    (remove-if-not #'evenp '(1) :end 2)
                    (delete-if #'evenp '(1) :end 2)
                    (delete-if-not #'evenp '(1) :end 2)
                    (substitute 2 1 '(1) :end 2)
                    (nsubstitute 2 1 '(1) :end 2)
                    (substitute-if 2 #'evenp '(1) :end 2)
                    (substitute-if-not 2 #'evenp '(1) :end 2)
                    (nsubstitute-if 2 #'evenp '(1) :end 2)
                    (nsubstitute-if-not 2 #'evenp '(1) :end 2)
                    (remove-duplicates '(1) :end 2)
                    (delete-duplicates '(1) :end 2)
                    (search '(1) '(1) :end1 2)
                    (mismatch '(1) '(1) :end1 2)
                    (fill (list 1) 0 :end 2)
                    (replace (list 1) (list 1) :end1 2)
                    (reduce #'+ '(1) :end 2)
                    (subseq '(1) 0 2)))))
    (every (lambda (r) (equal r '(:type-error :type-error))) results))
  t)
