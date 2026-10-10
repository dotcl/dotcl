;;; (REPLACE target source &key start1 end1 start2 end2) with literal keywords
;;; calls the runtime with the bounds in fixed positions.
;;;
;;; The call went through the global function: the symbol was looked up and
;;; the arguments were put in an array (72 to 88 bytes a call), and the
;;; keywords were read back out of it. ironclad's scrypt does a REPLACE of 64
;;; bytes, or of the 16-word Salsa20 state, several times a block. The
;;; arguments are still evaluated in the order written; a keyword that is not a
;;; literal, :ALLOW-OTHER-KEYS or a repeated keyword is the generic call.

(setf dotcl:*save-sil* t)

(defun %rk-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %rk-plain (a b) (replace a b))
(defun %rk-s1 (a b s) (replace a b :start1 s))
(defun %rk-e2-s2 (a b e s) (replace a b :end2 e :start2 s))
(defun %rk-all (a b s1 e1 s2 e2) (replace a b :start1 s1 :end1 e1 :start2 s2 :end2 e2))
;; The generic call: a repeated keyword, :ALLOW-OTHER-KEYS, a keyword variable.
(defun %rk-dup (a b) (replace a b :start1 1 :start1 2))
(defun %rk-aok (a b) (replace a b :allow-other-keys t :foo 1))
(defun %rk-var (a b k v) (replace a b k v))

(deftest-emitting-only replace-keys.sil
  (list (mapcar (lambda (f) (%rk-count "Runtime.ReplaceKeys" f))
                (list #'%rk-plain #'%rk-s1 #'%rk-e2-s2 #'%rk-all))
        (mapcar (lambda (f) (%rk-count "Runtime.ReplaceKeys" f))
                (list #'%rk-dup #'%rk-aok #'%rk-var)))
  ((1 1 1 1) (0 0 0)))

(defun %rk-u32 (n) (let ((v (make-array n :element-type '(unsigned-byte 32))))
                     (dotimes (i n v) (setf (aref v i) (+ #x10000000 i)))))

;; Every combination of the four bounds (NIL for an end included) gives what
;; the generic REPLACE gives, on numeric vectors, general vectors, strings and
;; lists.
(deftest replace-keys.same-as-generic
  (let ((bad '()))
    (dolist (mk (list (lambda () (%rk-u32 8))
                      (lambda () (coerce '(a b c d e f g h) 'vector))
                      (lambda () (copy-seq "abcdefgh"))
                      (lambda () (list 1 2 3 4 5 6 7 8))))
      (dolist (s1 '(0 2))
        (dolist (e1 '(nil 5 8))
          (dolist (s2 '(0 3))
            (dolist (e2 '(nil 4 8))
              (let ((x (funcall mk)) (y (reverse (funcall mk)))
                    (gx (funcall mk)) (gy (reverse (funcall mk))))
                (%rk-all x y s1 e1 s2 e2)
                (apply #'replace gx gy (list :start1 s1 :end1 e1 :start2 s2 :end2 e2))
                (unless (equalp x gx) (push (list x gx s1 e1 s2 e2) bad))))))))
    bad)
  nil)

(deftest replace-keys.values
  (let ((order '()))
    (flet ((note (tag v) (push tag order) v))
      (let* ((a (%rk-u32 6)) (b (%rk-u32 6)) (c (copy-seq "abcdef")))
        (list
         ;; Written order: :END2 before :START2.
         (replace (note :target (copy-seq "......")) (note :source "xyz")
                  :end2 (note :end2 3) :start2 (note :start2 1))
         (reverse order)
         (coerce (%rk-plain (make-array 3 :element-type '(unsigned-byte 32)) a) 'list)
         (%rk-s1 (copy-seq "abc") "XY" 2)
         (%rk-e2-s2 (copy-seq "abc") "XYZ" 3 1)
         ;; One vector, overlapping regions, both directions.
         (progn (replace b b :start1 1 :end2 5) (coerce b 'list))
         (progn (replace c c :start2 1) c)
         (%rk-dup (copy-seq "abc") "XY")
         (%rk-aok (copy-seq "abc") "XY")
         (%rk-var (copy-seq "abc") "XY" :start1 1)))))
  ("yz...." (:target :source :end2 :start2)
   (#x10000000 #x10000001 #x10000002) "abX" "YZc"
   (#x10000000 #x10000000 #x10000001 #x10000002 #x10000003 #x10000004)
   "bcdeff" "aXY" "XYc" "aXY"))

(deftest replace-keys.errors
  (flet ((kind (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error () :type-error))))
    (list (kind (lambda () (%rk-s1 (copy-seq "abc") "XY" -1)))
          (kind (lambda () (%rk-s1 (copy-seq "abc") "XY" :x)))
          (kind (lambda () (%rk-s1 (copy-seq "abc") "XY" 4)))
          (kind (lambda () (%rk-plain 1 "XY")))))
  (:type-error :type-error :type-error :type-error))

;; A REPLACE of two 16-word (unsigned-byte 32) vectors allocates nothing: the
;; generic call allocated its argument array every time.
(defun %rk-bytes () (nth 4 (dotcl:gc-stats)))
(deftest-emitting-only replace-keys.no-allocation
  (let ((a (%rk-u32 16)) (b (%rk-u32 16)))
    (%rk-plain a b)
    (let ((b0 (%rk-bytes)))
      (dotimes (i 10000) (%rk-plain a b))
      (< (- (%rk-bytes) b0) 10000)))
  t)

(setf dotcl:*save-sil* nil)
