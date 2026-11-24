;;; A function of the same file whose every result is a global proclaimed to hold one
;;; (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (256)) answers that type: a loop reading the table it returns
;;; takes unboxed elements instead of boxing each one.

(defun %iart-source (leaves)
  (format nil "(declaim (type (simple-array (unsigned-byte 32) (256)) *iart-a* *iart-b*))
(defvar *iart-a* (let ((a (make-array 256 :element-type '(unsigned-byte 32)))) (dotimes (i 256 a) (setf (aref a i) (logand #xffffffff (* i 2654435761))))))
(defvar *iart-b* (let ((a (make-array 256 :element-type '(unsigned-byte 32)))) (dotimes (i 256 a) (setf (aref a i) (+ i 1)))))
(defstruct iart-s (kind 0))
(defun iart-table (s) ~A)
(defun iart-sum (s n)
  (declare (fixnum n))
  (let ((crc 1))
    (declare (type (unsigned-byte 32) crc))
    (do ((i 0 (1+ i)) (table (iart-table s)))
        ((>= i n) crc)
      (declare (fixnum i))
      (setf crc (logxor (aref table (logand (logxor crc i) #xff)) (logand #xffffffff (ash crc -8)))))))
(defun iart-bytes (s n)
  (let ((b0 (dotnet:static \"System.GC\" \"GetAllocatedBytesForCurrentThread\")))
    (iart-sum s n)
    (- (dotnet:static \"System.GC\" \"GetAllocatedBytesForCurrentThread\") b0)))
" leaves))

(defun %iart-run (leaves)
  (let* ((src (regression-temp-file "iart.lisp"))
         (fasl (regression-temp-file "iart.fasl")))
    (with-open-file (o src :direction :output :if-exists :supersede)
      (write-string (%iart-source leaves) o))
    (load (compile-file src :output-file fasl))
    (let ((s (funcall (intern "MAKE-IART-S" :cl-user))))
      (funcall (intern "IART-SUM" :cl-user) s 20000)
      (list (funcall (intern "IART-SUM" :cl-user) s 20000)
            (funcall (intern "IART-BYTES" :cl-user) s 20000)))))

(deftest-compiled-only inferred-array-return-type.typed-loop-does-not-box
  (let ((typed (%iart-run "(if (zerop (iart-s-kind s)) *iart-a* *iart-a*)"))
        (untyped (%iart-run "(if (zerop (iart-s-kind s)) *iart-a* (identity *iart-a*))")))
    (list (= (car typed) (car untyped))
          (< (cadr typed) 20000)
          (> (cadr untyped) 100000)))
  (t t t))

(deftest-compiled-only inferred-array-return-type.other-table-by-kind
  (let ((r (%iart-run "(if (zerop (iart-s-kind s)) *iart-b* *iart-a*)")))
    (list (integerp (car r)) (< (cadr r) 20000)))
  (t t))

(deftest-compiled-only inferred-array-return-type.etypecase-and-case
  (let ((e (%iart-run "(etypecase s (iart-s *iart-a*))"))
        (c (%iart-run "(case (iart-s-kind s) (0 *iart-a*) (t *iart-b*))"))
        (u (%iart-run "(etypecase s (iart-s (identity *iart-a*)))")))
    (list (= (car e) (car u))
          (< (cadr e) 20000)
          (< (cadr c) 20000)
          (> (cadr u) 100000)))
  (t t t t))

(deftest-compiled-only inferred-array-return-type.values-and-other-shapes-are-not-inferred
  (let ((v (%iart-run "(values *iart-a* 1)"))
        (l (%iart-run "(let ((x *iart-a*)) x)")))
    (list (> (cadr v) 100000) (integerp (car l))))
  (t t))
