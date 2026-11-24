;;; DELETE-DUPLICATES on a vector with a fill pointer shrinks it in place and
;;; returns it, as DELETE does.

(deftest delete-duplicates-fill-pointer.in-place
  (let* ((v (make-array 5 :fill-pointer 5 :initial-contents '(0 1 2 1 3)))
         (r (delete-duplicates v)))
    (list (eq r v) (fill-pointer v) (coerce r 'list)))
  (t 4 (0 2 1 3)))

(deftest delete-duplicates-fill-pointer.from-end
  (let* ((v (make-array 5 :fill-pointer 5 :initial-contents '(1 2 1 3 2)))
         (r (delete-duplicates v :from-end t)))
    (list (eq r v) (fill-pointer v) (coerce r 'list)))
  (t 3 (1 2 3)))

(deftest delete-duplicates-fill-pointer.partial-fill
  (let* ((v (make-array 5 :fill-pointer 3 :initial-contents '(0 1 1 1 3)))
         (r (delete-duplicates v)))
    (list (eq r v) (fill-pointer v) (coerce r 'list)))
  (t 2 (0 1)))

(deftest delete-duplicates-fill-pointer.remove-leaves-original
  (let* ((v (make-array 5 :fill-pointer 5 :initial-contents '(0 1 2 1 3)))
         (r (remove-duplicates v)))
    (list (eq r v) (fill-pointer v)))
  (nil 5))
