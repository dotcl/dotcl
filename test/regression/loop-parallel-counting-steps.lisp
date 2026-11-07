;;; LOOP with two counting variables joined by AND, an array store whose value is a
;;; BLOCK over an IF with an ERROR arm, and what must stay parallel.

(defun %lpc-encode (src dest)
  (declare (type (simple-array character (*)) src)
           (type (simple-array (unsigned-byte 8) (*)) dest))
  (loop for i fixnum from 0 below (length src)
        and di fixnum from 0 do
        (setf (aref dest di)
              (let ((code (char-code (schar src i))))
                (declare (type (integer 0 #x10ffff) code))
                (block latin1 (if (< code 256) code (error "not latin-1: ~S" code)))))
        finally (return di)))

(deftest loop-parallel-counting-steps.encode
  (let* ((s (coerce '(#\a #\b #\z #\y) 'simple-string))
         (d (make-array 4 :element-type '(unsigned-byte 8))))
    (list (%lpc-encode s d) (coerce d 'list)
          (handler-case (%lpc-encode (coerce (list #\a (code-char 300)) '(simple-array character (*)))
                                     (make-array 2 :element-type '(unsigned-byte 8)))
            (error () :error))))
  (4 (97 98 122 121) :error))

(deftest loop-parallel-counting-steps.and-order
  (list
   (loop for i from 0 below 4 and j from 10 collect (list i j))
   (loop for i from 3 downto 0 and j from 0 by 2 collect (list i j))
   (let ((x 0)) (loop for a from 1 to 3 and b = (incf x) then (incf x 10) collect (list a b)))
   (loop for i from 0 below 3 and j = 0 then i collect (list i j))
   (let ((n 5)) (loop for i from 0 below n and k from 1 by n collect (list i k))))
  (((0 10) (1 11) (2 12) (3 13))
   ((3 0) (2 2) (1 4) (0 6))
   ((1 1) (2 11) (3 21))
   ((0 0) (1 0) (2 1))
   ((0 1) (1 6) (2 11) (3 16) (4 21))))

(defun %lpc-block-value (x)
  (declare (type (integer 0 100) x))
  (let ((v (block b (if (< x 50) x (error "big")))))
    (declare (type (integer 0 49) v))
    (* v 2)))

(defun %lpc-block-return (x)
  (declare (type (integer 0 100) x))
  (let ((v (block b (if (< x 50) (return-from b 7) x))))
    (+ v 1)))

(deftest loop-parallel-counting-steps.block-if-error
  (list (%lpc-block-value 10) (handler-case (%lpc-block-value 60) (error () :error))
        (%lpc-block-return 10) (%lpc-block-return 80))
  (20 :error 8 81))
