;;; A LET that binds several specials pushes them as one group.

(defvar *slgb-a* :a)
(defvar *slgb-b* :b)
(defvar *slgb-c* :c)
(defvar *slgb-d* :d)

(defun %slgb-read () (list *slgb-a* *slgb-b* *slgb-c* *slgb-d*))

(deftest special-let-group-bind.values
  (list (let ((*slgb-a* 1) (*slgb-b* 2) (*slgb-c* 3))
          (%slgb-read))
        (%slgb-read))
  ((1 2 3 :d) (:a :b :c :d)))

(deftest special-let-group-bind.nested
  (list (let ((*slgb-a* 1) (*slgb-b* 2))
          (list (let ((*slgb-a* 10) (*slgb-c* 30) (*slgb-d* 40))
                  (%slgb-read))
                (%slgb-read)))
        (%slgb-read))
  (((10 2 30 40) (1 2 :c :d)) (:a :b :c :d)))

(deftest special-let-group-bind.throw
  (list (catch 'slgb
          (let ((*slgb-a* 1) (*slgb-b* 2) (*slgb-c* 3))
            (throw 'slgb (%slgb-read))))
        (%slgb-read))
  ((1 2 3 :d) (:a :b :c :d)))

(deftest special-let-group-bind.return-from
  (list (block out
          (let ((*slgb-a* 1) (*slgb-b* 2) (*slgb-c* 3))
            (return-from out (%slgb-read))))
        (%slgb-read))
  ((1 2 3 :d) (:a :b :c :d)))

(deftest special-let-group-bind.error-in-body
  (list (handler-case
            (let ((*slgb-a* 1) (*slgb-b* 2) (*slgb-c* 3))
              (error "slgb"))
          (error () :caught))
        (%slgb-read))
  (:caught (:a :b :c :d)))

(deftest special-let-group-bind.error-in-init
  (list (handler-case
            (let ((*slgb-a* 1)
                  (*slgb-b* (error "slgb"))
                  (*slgb-c* 3))
              (%slgb-read))
          (error () :caught))
        (%slgb-read))
  (:caught (:a :b :c :d)))

(deftest special-let-group-bind.let-star-init-error
  (list (handler-case
            (let* ((*slgb-a* 1)
                   (*slgb-b* (error "slgb"))
                   (*slgb-c* 3))
              (%slgb-read))
          (error () :caught))
        (%slgb-read))
  (:caught (:a :b :c :d)))

(deftest special-let-group-bind.setq-and-progv
  (list (let ((*slgb-a* 1) (*slgb-b* 2))
          (setq *slgb-a* 10)
          (progv '(*slgb-c*) '(3) (%slgb-read)))
        (%slgb-read))
  ((10 2 3 :d) (:a :b :c :d)))

(defun %slgb-params (*slgb-a* *slgb-b* *slgb-c*)
  (%slgb-read))

(deftest special-let-group-bind.params
  (list (%slgb-params 1 2 3) (%slgb-read))
  ((1 2 3 :d) (:a :b :c :d)))

(deftest special-let-group-bind.param-throw
  (list (catch 'slgb
          (funcall (lambda (*slgb-a* *slgb-b*)
                     (throw 'slgb (%slgb-read)))
                   1 2))
        (%slgb-read))
  ((1 2 :c :d) (:a :b :c :d)))

(defvar *slgb-g0* 0)
(defvar *slgb-g1* 0)
(defvar *slgb-g2* 0)
(defvar *slgb-g3* 0)
(defvar *slgb-g4* 0)
(defvar *slgb-g5* 0)
(defvar *slgb-g6* 0)
(defvar *slgb-g7* 0)
(defvar *slgb-g8* 0)
(defvar *slgb-g9* 0)

(deftest special-let-group-bind.sizes
  (list
   (let ((*slgb-g0* 10) (*slgb-g1* 11))
     (list *slgb-g0* *slgb-g1*))
   (let ((*slgb-g0* 20) (*slgb-g1* 21) (*slgb-g2* 22) (*slgb-g3* 23))
     (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3*))
   (let ((*slgb-g0* 30) (*slgb-g1* 31) (*slgb-g2* 32) (*slgb-g3* 33)
         (*slgb-g4* 34))
     (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3* *slgb-g4*))
   (let ((*slgb-g0* 40) (*slgb-g1* 41) (*slgb-g2* 42) (*slgb-g3* 43)
         (*slgb-g4* 44) (*slgb-g5* 45))
     (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3* *slgb-g4* *slgb-g5*))
   (let ((*slgb-g0* 50) (*slgb-g1* 51) (*slgb-g2* 52) (*slgb-g3* 53)
         (*slgb-g4* 54) (*slgb-g5* 55) (*slgb-g6* 56))
     (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3* *slgb-g4* *slgb-g5*
           *slgb-g6*)))
  ((10 11) (20 21 22 23) (30 31 32 33 34) (40 41 42 43 44 45)
   (50 51 52 53 54 55 56)))

(deftest special-let-group-bind.wide
  (list (let ((*slgb-g0* 0) (*slgb-g1* 1) (*slgb-g2* 2) (*slgb-g3* 3)
              (*slgb-g4* 4) (*slgb-g5* 5) (*slgb-g6* 6) (*slgb-g7* 7)
              (*slgb-g8* 8) (*slgb-g9* 9))
          (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3* *slgb-g4*
                *slgb-g5* *slgb-g6* *slgb-g7* *slgb-g8* *slgb-g9*))
        (list *slgb-g0* *slgb-g9*))
  ((0 1 2 3 4 5 6 7 8 9) (0 0)))

(defun %slgb-count (needle text)
  (loop with n = 0 and start = 0
        for p = (search needle text :start2 start)
        while p do (incf n) (setf start (+ p (length needle)))
        finally (return n)))

(defvar *slgb-saved-save-sil2* dotcl:*save-sil*)
(setf dotcl:*save-sil* t)
(defun %slgb-two (a b)
  (let ((*slgb-a* a) (*slgb-b* b))
    (%slgb-read)))
(defun %slgb-two-params (*slgb-a* *slgb-b*)
  (%slgb-read))
(setf dotcl:*save-sil* *slgb-saved-save-sil2*)

(deftest-emitting-only special-let-group-bind.two-per-binding
  (let ((s (%flat-text (dotcl:function-sil #'%slgb-two)))
        (p (%flat-text (dotcl:function-sil #'%slgb-two-params))))
    (list (%slgb-two 1 2)
          (%slgb-two-params 3 4)
          (%slgb-count "DynamicBindings.PushOn" s)
          (search "DynamicBindings.PushGroup2On" s)
          (search "DynamicBindings.BindGroupDepthOn" s)
          (and (search "DynamicBindings.BindDepthOn" s) t)
          (%slgb-count "DynamicBindings.PushOn" p)
          (search "DynamicBindings.PushGroup2On" p)))
  ((1 2 :c :d) (3 4 :c :d) 2 nil nil t 2 nil))

(deftest special-let-group-bind.runtime-entries
  (let ((db "DotCL.DynamicBindings"))
    (list
     (let ((d (dotnet:static db "BindDepth")))
       (dotnet:static db "PushGroup2" '*slgb-g0* 10 '*slgb-g1* 11)
       (prog1 (list *slgb-g0* *slgb-g1*)
         (dotnet:static db "UnbindTo" d)))
     (let ((d (dotnet:static db "BindDepth")))
       (dotnet:static db "PushGroup3" '*slgb-g0* 20 '*slgb-g1* 21 '*slgb-g2* 22)
       (prog1 (list *slgb-g0* *slgb-g1* *slgb-g2*)
         (dotnet:static db "UnbindTo" d)))
     (let ((d (dotnet:static db "BindDepth")))
       (dotnet:static db "PushGroup8"
                      '*slgb-g0* 30 '*slgb-g1* 31 '*slgb-g2* 32 '*slgb-g3* 33
                      '*slgb-g4* 34 '*slgb-g5* 35 '*slgb-g6* 36 '*slgb-g7* 37)
       (prog1 (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3*
                    *slgb-g4* *slgb-g5* *slgb-g6* *slgb-g7*)
         (dotnet:static db "UnbindTo" d)))
     (list *slgb-g0* *slgb-g1* *slgb-g2* *slgb-g3*
           *slgb-g4* *slgb-g5* *slgb-g6* *slgb-g7*)))
  ((10 11) (20 21 22) (30 31 32 33 34 35 36 37)
   (0 0 0 0 0 0 0 0)))

(defvar *slgb-saved-save-sil* dotcl:*save-sil*)
(setf dotcl:*save-sil* t)
(defun %slgb-shape (a b c)
  (let ((*slgb-a* a) (*slgb-b* b) (*slgb-c* c))
    (%slgb-read)))
(setf dotcl:*save-sil* *slgb-saved-save-sil*)

(deftest-emitting-only special-let-group-bind.group-shape
  (let ((s (%flat-text (dotcl:function-sil #'%slgb-shape))))
    (list (%slgb-shape 1 2 3)
          (and (search "DynamicBindings.PushGroup3On" s) t)
          (search "DynamicBindings.PushOn" s)
          (and (search "DynamicBindings.BindGroupDepthOn" s) t)
          (search "DynamicBindings.BindDepthOn" s)
          (and (search "DynamicBindings.UnbindToOn" s) t)))
  ((1 2 3 :d) t nil t nil t))
