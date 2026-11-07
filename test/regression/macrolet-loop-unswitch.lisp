;;; A loop written inside a MACROLET template counts as a loop when deciding
;;; to fetch and test an array's element buffer once before it.
;;;
;;; ironclad's XOR-BLOCK builds its byte loop through a local macro, so the
;;; source of the function shows the array reads but no LOOP around them. The
;;; reads kept a buffer test at each access (an ABFALL arm per read) and the
;;; loop ran at 1.4 ns/byte; with the test moved in front of the loop it runs
;;; about 40% faster.

(setf dotcl:*save-sil* t)

(defun %mlu-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %mlu-xor (len a as b bs c cs)
  (declare (type (simple-array (unsigned-byte 8) (*)) a b c)
           (type (integer 0 1000000) len as bs cs)
           (optimize speed (safety 0)))
  (macrolet ((xor-bytes (size form)
               `(loop until (< len ,size) do
                  ,form
                  (incf cs ,size) (incf as ,size) (incf bs ,size)
                  (decf len ,size))))
    (xor-bytes 1 (setf (aref c cs) (logxor (aref a as) (aref b bs)))))
  (values))

;;; The same macro with no loop in it: the reads stay as they were.
(defun %mlu-once (a as c cs)
  (declare (type (simple-array (unsigned-byte 8) (*)) a c)
           (type (integer 0 1000000) as cs)
           (optimize speed (safety 0)))
  (macrolet ((copy1 (form) form))
    (copy1 (setf (aref c cs) (aref a as))))
  (values))

(deftest macrolet-loop-unswitch.values
  (let ((a (make-array 70 :element-type '(unsigned-byte 8)))
        (b (make-array 70 :element-type '(unsigned-byte 8)))
        (c (make-array 70 :element-type '(unsigned-byte 8) :initial-element 255)))
    (dotimes (i 70) (setf (aref a i) (mod (* i 7) 256) (aref b i) (mod (* i 13) 256)))
    (%mlu-xor 64 a 3 b 2 c 1)
    (list (aref c 0) (aref c 1) (aref c 64) (aref c 65)
          (= (aref c 1) (logxor (aref a 3) (aref b 2)))
          (= (aref c 64) (logxor (aref a 66) (aref b 65)))))
  (255 #.(logxor (mod (* 3 7) 256) (mod (* 2 13) 256))
   #.(logxor (mod (* 66 7) 256) (mod (* 65 13) 256)) 255 t t))

(deftest-emitting-only macrolet-loop-unswitch.sil
  (list (%mlu-count "ABFALL" #'%mlu-xor)
        (plusp (%mlu-count "ABUSLOW" #'%mlu-xor))
        (plusp (%mlu-count "ABFALL" #'%mlu-once)))
  (0 t t))

(setf dotcl:*save-sil* nil)
