;;; STABLE-SORT must preserve the original order of elements the predicate
;;; considers equal (CLHS 17.3). It used to share SORT's Array.Sort/List.Sort
;;; path (introsort — unstable), so equal keys came out in an arbitrary order.
;;;
;;; The lists here are long enough that the underlying introsort actually
;;; reorders equal elements; a 2- or 3-element case can pass by accident.

(defun ss-tagged (n groups)
  "List of (key . tag) with keys cycling over GROUPS and tags 0..N-1."
  (let ((r '()))
    (dotimes (i n (nreverse r))
      (push (cons (mod i groups) i) r))))

(defun ss-tags-per-key (sorted)
  "Tags of SORTED grouped in encounter order — must be ascending within a key."
  (mapcar #'cdr sorted))

(defun ss-ascending-within-keys-p (sorted)
  (let ((last-key nil) (last-tag nil) (ok t))
    (dolist (e sorted ok)
      (when (and last-key (eql (car e) last-key) (> last-tag (cdr e)))
        (setf ok nil))
      (setf last-key (car e) last-tag (cdr e)))))

(deftest stable-sort.list-equal-keys-keep-order
  (ss-ascending-within-keys-p
   (stable-sort (ss-tagged 60 3) #'< :key #'car))
  t)

(deftest stable-sort.list-all-keys-equal-is-identity
  (ss-tags-per-key (stable-sort (ss-tagged 40 1) #'< :key #'car))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
   20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39))

(deftest stable-sort.vector-equal-keys-keep-order
  (ss-ascending-within-keys-p
   (coerce (stable-sort (coerce (ss-tagged 60 3) 'vector) #'< :key #'car) 'list))
  t)

;;; No :key — the predicate itself declares elements equal.
(deftest stable-sort.no-key-equal-elements-keep-order
  (ss-ascending-within-keys-p
   (stable-sort (ss-tagged 50 2) (lambda (a b) (< (car a) (car b)))))
  t)

;;; Sorting is still correct, not just stable.
(deftest stable-sort.keys-are-sorted
  (mapcar #'car (stable-sort (ss-tagged 9 3) #'< :key #'car))
  (0 0 0 1 1 1 2 2 2))

;;; SORT itself keeps its (unstable but conforming) behaviour and must still
;;; produce a correctly ordered result.
(deftest stable-sort.sort-still-orders
  (sort (list 5 3 9 1 7) #'<)
  (1 3 5 7 9))
