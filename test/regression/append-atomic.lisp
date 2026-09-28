;;; OPEN :IF-EXISTS :APPEND gives every write the then-current end of file.
;;;
;;; It used to seek to the end once, at open, and then write at the offset the
;;; stream tracked itself. Two appenders to one file (two streams here, or two
;;; processes, or two threads logging) each wrote at their own stale offset and
;;; overwrote the other: the interleaving test below lost 100 of its 201 lines.
;;; The OS now keeps the stream at end of file (O_APPEND on Unix, an append-only
;;; handle on Windows).
;;;
;;; Two streams in one process show the same loss as two processes, and do it
;;; deterministically, so that is what is tested.

(defparameter *app-dir*
  (regression-temp-dir))
(defun app-path (name)
  (format nil "~a/dotcl-append-~a-~d.txt" *app-dir* name (random 1000000000)))

(defun app-count-lines (path)
  (with-open-file (s path)
    (loop for l = (read-line s nil) while l collect l)))

;;; Two streams opened on an existing file, writes interleaved and flushed.
(deftest append-two-streams-interleaved
  (let ((f (app-path "interleave")))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :if-exists :supersede)
             (write-line "start" s))
           (let ((a (open f :direction :output :if-exists :append))
                 (b (open f :direction :output :if-exists :append)))
             (unwind-protect
                  (dotimes (i 50)
                    (write-line (format nil "a~d" i) a) (finish-output a)
                    (write-line (format nil "b~d" i) b) (finish-output b))
               (close a) (close b)))
           (let ((lines (app-count-lines f)))
             (list (length lines) (first lines) (second lines) (third lines)
                   (car (last lines)))))
      (ignore-errors (delete-file f))))
  (101 "start" "a0" "b0" "b49"))

;;; The missing-file case: :IF-DOES-NOT-EXIST :CREATE used to take the
;;; truncating path, so the second opener erased what the first had written.
(deftest append-create-does-not-truncate
  (let ((f (app-path "create")))
    (unwind-protect
         (let ((a (open f :direction :output :if-exists :append
                          :if-does-not-exist :create)))
           (write-line "first" a) (finish-output a)
           (let ((b (open f :direction :output :if-exists :append
                            :if-does-not-exist :create)))
             (write-line "second" b) (finish-output b)
             (close b))
           (write-line "third" a)
           (close a)
           (app-count-lines f))
      (ignore-errors (delete-file f))))
  ("first" "second" "third"))

;;; FILE-POSITION reports the real end of file, including what another writer
;;; appended. Repositioning anywhere but the end fails (NIL) rather than
;;; answering T and then writing somewhere else: every write goes to the end.
(deftest append-file-position
  (let ((f (app-path "fpos")))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :if-exists :supersede)
             (write-string "0123456789" s))
           (with-open-file (a f :direction :output :if-exists :append)
             (let ((p0 (file-position a)))
               (with-open-file (b f :direction :output :if-exists :append)
                 (write-string "xy" b))
               (let ((p1 (file-position a))
                     (to-start (file-position a :start))
                     (to-end (file-position a :end)))
                 (write-string "ab" a)
                 (finish-output a)
                 (list p0 p1 to-start to-end (file-position a) (file-length a))))))
      (ignore-errors (delete-file f))))
  (10 12 nil t 14 14))

(deftest append-file-position-start-still-appends
  (let ((f (app-path "fstart")))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :if-exists :supersede)
             (write-string "0123456789" s))
           (with-open-file (a f :direction :output :if-exists :append)
             (file-position a :start)
             (write-string "ab" a))
           (with-open-file (s f) (values (read-line s))))
      (ignore-errors (delete-file f))))
  "0123456789ab")
