;;; A synonym stream answers stream queries as its target does.

(defvar *ssq-target* nil)
(defvar *ssq-mid* nil)

(deftest synonym-stream-queries.file-position
  (let ((path (regression-temp-file "synonym-fp.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "abcdef" s)
      (force-output s)
      (let ((*ssq-target* s))
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (list (file-position syn) (file-position s))))))
  (6 6))

(deftest synonym-stream-queries.file-position-set
  (let ((path (regression-temp-file "synonym-fp-set.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string "abcdef" s)
      (force-output s)
      (let ((*ssq-target* s))
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (list (file-position syn 2)
                (file-position syn)
                (file-position s))))))
  (t 2 2))

(deftest synonym-stream-queries.file-position-rebind
  (let ((p1 (regression-temp-file "synonym-fp-a.txt"))
        (p2 (regression-temp-file "synonym-fp-b.txt")))
    (with-open-file (s1 p1 :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
      (write-string "ab" s1)
      (force-output s1)
      (with-open-file (s2 p2 :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
        (write-string "abcdef" s2)
        (force-output s2)
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (list (let ((*ssq-target* s1)) (file-position syn))
                (let ((*ssq-target* s2)) (file-position syn)))))))
  (2 6))

(deftest synonym-stream-queries.stream-external-format
  (let ((path (regression-temp-file "synonym-ef.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :latin-1)
      (let ((*ssq-target* s))
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (equal (stream-external-format syn)
                 (stream-external-format s))))))
  t)

(deftest synonym-stream-queries.stream-external-format-rebind
  (let ((p1 (regression-temp-file "synonym-ef-a.txt"))
        (p2 (regression-temp-file "synonym-ef-b.txt")))
    (with-open-file (s1 p1 :direction :output :if-exists :supersede
                           :if-does-not-exist :create :external-format :latin-1)
      (with-open-file (s2 p2 :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (list (let ((*ssq-target* s1))
                  (equal (stream-external-format syn)
                         (stream-external-format s1)))
                (let ((*ssq-target* s2))
                  (equal (stream-external-format syn)
                         (stream-external-format s2))))))))
  (t t))

(deftest synonym-stream-queries.stream-external-format-nested
  (let ((path (regression-temp-file "synonym-ef-nested.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :utf-8)
      (let ((*ssq-target* s)
            (*ssq-mid* (make-synonym-stream '*ssq-target*)))
        (let ((syn (make-synonym-stream '*ssq-mid*)))
          (equal (stream-external-format syn)
                 (stream-external-format s))))))
  t)

(deftest synonym-stream-queries.file-string-length
  (let ((path (regression-temp-file "synonym-fsl.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :latin-1)
      (let ((*ssq-target* s))
        (let ((syn (make-synonym-stream '*ssq-target*)))
          (equal (file-string-length syn "abc")
                 (file-string-length s "abc"))))))
  t)

(deftest synonym-stream-queries.file-string-length-nested
  (let ((path (regression-temp-file "synonym-fsl-nested.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :utf-8)
      (let ((*ssq-target* s)
            (*ssq-mid* (make-synonym-stream '*ssq-target*)))
        (let ((syn (make-synonym-stream '*ssq-mid*)))
          (equal (file-string-length syn "abc")
                 (file-string-length s "abc"))))))
  t)

(deftest synonym-stream-queries.broadcast-stream-external-format-empty
  (stream-external-format (make-broadcast-stream))
  :default)

(deftest synonym-stream-queries.broadcast-stream-external-format-last
  (let ((p1 (regression-temp-file "synonym-bc-a.txt"))
        (p2 (regression-temp-file "synonym-bc-b.txt")))
    (with-open-file (a p1 :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :latin-1)
      (with-open-file (b p2 :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
        (let ((bc (make-broadcast-stream a b)))
          (list (equal (stream-external-format bc)
                       (stream-external-format b))
                (equal (stream-external-format bc)
                       (stream-external-format a)))))))
  (t nil))
