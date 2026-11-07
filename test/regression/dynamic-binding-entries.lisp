;;; The binding stack keeps each entry's symbol as its BindIndex. Nested bindings of
;;; every group size, unwinding by throw / error / return-from, PROGV with fresh
;;; symbols, and the values another thread sees must all come out as before.

(defvar *dbe-1* 1) (defvar *dbe-2* 2) (defvar *dbe-3* 3) (defvar *dbe-4* 4)
(defvar *dbe-5* 5) (defvar *dbe-6* 6) (defvar *dbe-7* 7) (defvar *dbe-8* 8)

(defun %dbe-read () (list *dbe-1* *dbe-2* *dbe-3* *dbe-4* *dbe-5* *dbe-6* *dbe-7* *dbe-8*))

(deftest dynamic-binding-entries.nested
  (list
   (let ((*dbe-1* 10)) (%dbe-read))
   (let ((*dbe-1* 10) (*dbe-2* 20)) (let ((*dbe-1* 11)) (%dbe-read)))
   (let ((*dbe-1* 10) (*dbe-2* 20) (*dbe-3* 30)) (let ((*dbe-2* 21) (*dbe-3* 31)) (%dbe-read)))
   (let ((*dbe-1* 10) (*dbe-2* 20) (*dbe-3* 30) (*dbe-4* 40))
     (let ((*dbe-1* 11) (*dbe-4* 41)) (%dbe-read)))
   (let ((*dbe-1* 10) (*dbe-2* 20) (*dbe-3* 30) (*dbe-4* 40) (*dbe-5* 50) (*dbe-6* 60)
         (*dbe-7* 70) (*dbe-8* 80))
     (let ((*dbe-8* 81)) (%dbe-read)))
   (%dbe-read))
  ((10 2 3 4 5 6 7 8) (11 20 3 4 5 6 7 8) (10 21 31 4 5 6 7 8) (11 20 30 41 5 6 7 8)
   (10 20 30 40 50 60 70 81) (1 2 3 4 5 6 7 8)))

(deftest dynamic-binding-entries.unwind
  (list
   (catch 'x (let ((*dbe-1* 10) (*dbe-2* 20)) (let ((*dbe-3* 30)) (throw 'x (%dbe-read)))))
   (%dbe-read)
   (handler-case (let ((*dbe-1* 10) (*dbe-2* 20) (*dbe-3* 30)) (error "e")) (error () (%dbe-read)))
   (block b (let ((*dbe-4* 40) (*dbe-5* 50)) (return-from b (%dbe-read))))
   (%dbe-read)
   (let ((*dbe-1* 100)) (unwind-protect (let ((*dbe-1* 200)) (%dbe-read)) (setq *dbe-2* 22)))
   (progn (setq *dbe-2* 2) (%dbe-read)))
  ((10 20 30 4 5 6 7 8) (1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8) (1 2 3 40 50 6 7 8)
   (1 2 3 4 5 6 7 8) (200 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8)))

(deftest dynamic-binding-entries.progv
  (let* ((syms (loop repeat 40 collect (make-symbol "S")))
         (r (progv syms (loop for i below 40 collect i)
              (list (symbol-value (first syms)) (symbol-value (nth 39 syms))
                    (let ((*dbe-1* 5)) (list *dbe-1* (symbol-value (nth 7 syms))))))))
    (list r (boundp (first syms)) (%dbe-read)))
  ((0 39 (5 7)) nil (1 2 3 4 5 6 7 8)))

(require "dotcl-thread")

(deftest dynamic-binding-entries.threads
  (let ((ths (loop for i below 4
                   collect (let ((i i))
                             (dotcl-thread:make-thread
                              (lambda ()
                                (let ((*dbe-1* (* 100 i)) (*dbe-2* i))
                                  (loop repeat 200 do (let ((*dbe-1* (1+ *dbe-1*))) (setq *dbe-1* (1- *dbe-1*))))
                                  (list *dbe-1* *dbe-2* *dbe-3*))))))))
    (mapcar #'dotcl-thread:thread-join ths))
  ((0 0 3) (100 1 3) (200 2 3) (300 3 3)))
