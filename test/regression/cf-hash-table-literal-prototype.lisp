;;; A large hash table literal in a fasl is built from a prototype the fasl
;;; keeps, and a table that holds mostly the same entries as one emitted before
;;; it in the file is built as a copy of that one's prototype plus the
;;; differences. A code walker that copies its environment tables for every
;;; nested scope (cl-environments, under generic-cl) put the same thousand
;;; entries into the fasl as IL once per scope: 14 MB for one source file,
;;; all of it JIT-compiled at load to run once.
;;;
;;; Each literal is still its own table.

(defun %chtp-compile (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(defvar *chtp-base*
  (let ((h (make-hash-table :test 'equal)))
    (dotimes (i 400) (setf (gethash (list 'chtp-k i) h) (list i 'chtp-v)))
    h))

(defun %chtp-variant (n)
  (let ((h (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (setf (gethash k h) v)) *chtp-base*)
    (setf (gethash (format nil "extra-~d" n) h) n)
    (remhash (list 'chtp-k n) h)
    (setf (gethash (list 'chtp-k (+ 100 n)) h) (list :changed n))
    h))

(defvar *chtp-tables* (list* *chtp-base* (loop for n below 5 collect (%chtp-variant n))))

(defmacro chtp-lit (i) `',(nth i *chtp-tables*))

(defun %chtp-forms (count)
  (loop for i below count
        collect `(defun ,(intern (format nil "CHTP-G~d" i)) () (chtp-lit ,i))))

(defun %chtp-size (fasl)
  (with-open-file (s fasl :element-type '(unsigned-byte 8)) (file-length s)))

(deftest-compiled-only cf-hash-table-literal-prototype.contents
  (progn
    (load (%chtp-compile "chtp-a" (%chtp-forms 6)))
    (let ((g0 (funcall (intern "CHTP-G0"))) (g1 (funcall (intern "CHTP-G1")))
          (g3 (funcall (intern "CHTP-G3"))))
      (list (hash-table-count g0) (hash-table-count g1)
            (hash-table-test g1)
            ;; CHTP-G3 returns variant 2.
            (gethash "extra-2" g3)
            (nth-value 1 (gethash (list 'chtp-k 2) g3))
            (gethash (list 'chtp-k 2) g0)
            (gethash (list 'chtp-k 102) g3)
            (gethash (list 'chtp-k 104) g3)
            (eq g1 g3)
            (eq g1 (funcall (intern "CHTP-G1"))))))
  (400 400 equal 2 nil (2 chtp-v) (:changed 2) (104 chtp-v) nil t))

;; An entry the compile-time tables shared is one object in the loaded ones.
(deftest-compiled-only cf-hash-table-literal-prototype.shared-entries
  (progn
    (load (%chtp-compile "chtp-b" (%chtp-forms 3)))
    (eq (gethash (list 'chtp-k 7) (funcall (intern "CHTP-G1")))
        (gethash (list 'chtp-k 7) (funcall (intern "CHTP-G2")))))
  t)

;; Six tables that are nearly the same cost little more than one.
(deftest-compiled-only cf-hash-table-literal-prototype.size
  (let ((one (%chtp-size (%chtp-compile "chtp-one" (%chtp-forms 1))))
        (six (%chtp-size (%chtp-compile "chtp-six" (%chtp-forms 6)))))
    (< six (* 2 one)))
  t)

;; EQ table with cons keys: an entry that changed cannot be found again by a
;; rebuilt key, so such a table is built in full; the contents are the same.
(defvar *chtp-eq-key* (list 'shared-key))
(defvar *chtp-eq-tables*
  (let ((a (make-hash-table :test 'eq)))
    (dotimes (i 100) (setf (gethash (intern (format nil "CHTP-S~d" i)) a) i))
    (setf (gethash *chtp-eq-key* a) :old)
    (let ((b (make-hash-table :test 'eq)))
      (maphash (lambda (k v) (setf (gethash k b) v)) a)
      (setf (gethash *chtp-eq-key* b) :new)
      (list a b))))
(defmacro chtp-eq-lit (i) `',(nth i *chtp-eq-tables*))

(deftest-compiled-only cf-hash-table-literal-prototype.eq-cons-key
  (progn
    (load (%chtp-compile "chtp-eq" '((defun chtp-eq0 () (chtp-eq-lit 0))
                                     (defun chtp-eq1 () (chtp-eq-lit 1)))))
    (let ((b (funcall (intern "CHTP-EQ1"))))
      (list (hash-table-count b)
            (let (vals) (maphash (lambda (k v) (when (consp k) (push v vals))) b) vals)
            (gethash (intern "CHTP-S5") b))))
  (101 (:new) 5))
