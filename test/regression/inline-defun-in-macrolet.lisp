;;; An INLINE function defined inside a top-level MACROLET and using its macros
;;; is substituted at call sites, inside the MACROLET and outside it, with the
;;; macros it was written against. (ironclad's BLOWFISH-F reads its S-boxes
;;; through such macros and was called, not substituted, sixteen times per
;;; block.) Also: a call to an inline function in an ARGUMENT of a call to the
;;; same function, (add32 (add32 a b) c), is substituted too; the recursion
;;; guard covers the body of the expansion, not its arguments.

(setf dotcl:*save-sil* t)

(defun %idm-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %idm-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (inline %idm-add32))
(defun %idm-add32 (a b)
  (declare (type (unsigned-byte 32) a b))
  (logand (+ a b) #xffffffff))

(macrolet ((box (s which index) `(aref ,s (+ (* 4 ,which) ,index)))
           (box-1 (s index) `(box ,s 1 ,index)))
  (declaim (inline %idm-f))
  (defun %idm-f (x s)
    (declare (type (unsigned-byte 32) x)
             (type (simple-array (unsigned-byte 32) (*)) s))
    (%idm-add32 (box-1 s (logand x 3)) (%idm-add32 (box s 0 (logand x 3)) x)))
  (defun %idm-inside (x s)
    (%idm-f x s)))

(defun %idm-outside (x s)
  (%idm-f x s))

;; The caller binds a macro of the same name as one the body uses: the
;; substituted body still means its own.
(defun %idm-shadowed (x s)
  (macrolet ((box-1 (s index) (declare (ignore s index)) 1000))
    (+ (box-1 s 0) (%idm-f x s))))

;; A recursive inline function terminates.
(declaim (inline %idm-sum))
(defun %idm-sum (n)
  (if (<= n 0) 0 (+ n (%idm-sum (1- n)))))
(defun %idm-sum10 () (%idm-sum 10))

(defparameter *idm-s*
  (make-array 8 :element-type '(unsigned-byte 32)
                :initial-contents '(1 2 3 4 10 20 30 #xffffffff)))

(deftest inline-defun-in-macrolet.values
  (list (%idm-inside 2 *idm-s*)
        (%idm-outside 2 *idm-s*)
        (%idm-outside 3 *idm-s*)
        (%idm-shadowed 1 *idm-s*)
        (%idm-sum10))
  (35 35 6 1023 55))

(deftest-emitting-only inline-defun-in-macrolet.substituted
  (list (%idm-count "%IDM-F" (%idm-sil #'%idm-outside))
        (%idm-count "%IDM-F" (%idm-sil #'%idm-inside))
        (%idm-count "%IDM-ADD32" (%idm-sil #'%idm-outside)))
  (0 0 0))

;; Wanted as a raw integer (an operand of LOGXOR assigned to a typed local),
;; the substituted body, MACROLET and all, gives its value unboxed.
(defun %idm-mix (n s)
  (declare (fixnum n) (type (simple-array (unsigned-byte 32) (*)) s))
  (let ((l 1) (r 2))
    (declare (type (unsigned-byte 32) l r))
    (dotimes (i n)
      (setf r (logxor r (%idm-f l s))
            l (logxor l (%idm-f r s))))
    (+ l r)))

(defun %idm-mix-called (n s)
  (declare (notinline %idm-f))
  (let ((l 1) (r 2))
    (dotimes (i n)
      (setf r (logxor r (%idm-f l s))
            l (logxor l (%idm-f r s))))
    (+ l r)))

(deftest inline-defun-in-macrolet.raw-value
  (list (%idm-mix 3 *idm-s*) (%idm-mix 7 *idm-s*)
        (%idm-mix-called 3 *idm-s*) (%idm-mix-called 7 *idm-s*))
  (83 319 83 319))

(defparameter *idm-big*
  (make-array 8 :element-type '(unsigned-byte 32)
                :initial-contents '(#x80000001 #x90000002 #xa0000003 #xb0000004
                                    #xc0000005 #xd0000006 #xe0000007 #xf0000008)))

;; The values are past the small-integer cache, so a boxed result would cost an
;; allocation per call.
(deftest-compiled-only inline-defun-in-macrolet.raw-value-no-consing
  (< (bytes-per-op (lambda (n) (%idm-mix n *idm-big*))) 4)
  t)
