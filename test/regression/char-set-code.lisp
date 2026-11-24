;;; (SETF (CHAR S I) (CODE-CHAR X)) with fixnum I and X is one raw store; every
;;; string kind, subscript and code gives what the two-step form gives.

(setf dotcl:*save-sil* t)

(defun %csc-set (s i x)
  (declare (fixnum i x))
  (setf (char s i) (code-char x)))

(defun %csc-set-drop (s i x)
  (declare (fixnum i x))
  (setf (char s i) (code-char x))
  s)

(defun %csc-fill (n)
  (let ((s (make-string n :initial-element #\a)))
    (dotimes (i (length s)) (setf (char s i) (code-char (+ 97 (mod i 26)))))
    s))

(defun %csc-try (thunk)
  (handler-case (funcall thunk) (error (e) (if (typep e 'type-error) :type-error (type-of e)))))

(defun %csc-sil (fn) (%flat-text (dotcl:function-sil fn)))

(deftest-emitting-only char-set-code.raw-store-shape
  (let ((text (%csc-sil #'%csc-fill)))
    (list (and (search "CharSetCodeVoidL" text) t)
          (and (search "CodeCharL" text) t)))
  (t nil))

(deftest char-set-code.values
  (let ((s (make-string 5 :initial-element #\-))
        (b (make-string 5 :initial-element #\- :element-type 'base-char))
        (a (make-array 5 :element-type 'character :initial-element #\- :adjustable t))
        (f (make-array 5 :element-type 'character :initial-element #\- :fill-pointer 3))
        (base (make-string 8 :initial-element #\-)))
    (let ((d (make-array 4 :element-type 'character :displaced-to base :displaced-index-offset 2)))
      (list (char-code (%csc-set s 0 97)) (char-code (%csc-set s 4 233)) (char-code (%csc-set s 2 65535))
            (map 'list #'char-code s)
            (%csc-set-drop b 1 66)
            (%csc-try (lambda () (%csc-set s 5 97)))
            (%csc-try (lambda () (%csc-set s -1 97)))
            (%csc-try (lambda () (%csc-set s 0 -1)))
            (%csc-try (lambda () (%csc-set nil 0 97)))
            (%csc-try (lambda () (%csc-set (vector 1 2) 0 97)))
            (progn (%csc-set a 1 120) (char-code (char a 1)))
            (progn (%csc-set f 2 121) (char-code (char f 2)))
            (%csc-try (lambda () (char-code (%csc-set f 4 121))))
            (progn (%csc-set d 1 122) (char-code (char base 3)))
            (%csc-fill 30))))
  (97 233 65535 (97 45 65535 45 233) "-B---" :type-error :type-error :type-error
   :type-error :type-error 120 121 121 122 "abcdefghijklmnopqrstuvwxyzabcd"))
