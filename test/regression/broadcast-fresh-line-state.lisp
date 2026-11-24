;;; regression: skip-under-interp
;;; FRESH-LINE on a broadcast stream and on its component share one line state.

(deftest broadcast-fresh-line-state.component-written-directly
  (let* ((s (make-string-output-stream)) (b (make-broadcast-stream s)))
    (write-string "ab" s)
    (list (fresh-line b) (get-output-stream-string s)))
  (t #.(format nil "ab~%")))

(deftest broadcast-fresh-line-state.written-through-broadcast
  (let* ((s (make-string-output-stream)) (b (make-broadcast-stream s)))
    (write-string "ab" b)
    (list (fresh-line s) (fresh-line b) (get-output-stream-string s)))
  (t nil #.(format nil "ab~%")))

(deftest broadcast-fresh-line-state.first-component-decides
  (let* ((s1 (make-string-output-stream)) (s2 (make-string-output-stream))
         (b (make-broadcast-stream s1 s2)))
    (write-string "x" s1)
    (list (fresh-line b) (get-output-stream-string s1) (get-output-stream-string s2)))
  (t #.(format nil "x~%") #.(format nil "~%")))

(deftest broadcast-fresh-line-state.file-component
  (let ((p (regression-temp-file "broadcast-fresh-line-state.txt")))
    (with-open-file (c p :direction :output :if-exists :supersede)
      (let ((b (make-broadcast-stream c)))
        (write-string "ab" c)
        (list (fresh-line b) (fresh-line c)))))
  (t nil))

(deftest broadcast-fresh-line-state.no-components
  (fresh-line (make-broadcast-stream))
  nil)
