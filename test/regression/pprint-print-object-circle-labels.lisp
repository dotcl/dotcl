;;; With *print-circle* an object printed again at its column (see pprint-print-object-prints-list-in-list)
;;; keeps every label it takes: its own #n= and those of what it holds, and the layout is done for the
;;; column after its label. The expected texts are SBCL's, for seven shapes at six right margins.

(defstruct (pcn (:print-object (lambda (o s) (format s "#S<~S>" (pcn-v o))))) v)

(defclass pcc () ((v :initarg :v :initform 0)))

(defmethod print-object ((o pcc) s) (format s "#<CNT ~A>" (slot-value o 'v)))

(defun pcn-form (i)
  (let* ((l (list 1 2 3)) (s (make-pcn :v l)) (c (make-instance 'pcc :v 9)) (m (list 4 5)))
    (ecase i
      (1 (list s s l l))
      (2 (list c c (list c)))
      (3 (list (list c) c c c c))
      (4 (list s (make-pcn :v m) m s l m))
      (5 (list l (list s) s (list l)))
      (6 (list s (list s s) (list c (list c)) c l))
      (7 (list 0 s c s c)))))

(defun pcn-print (i margin)
  (let ((*print-pretty* t) (*print-circle* t) (*print-right-margin* margin))
    (prin1-to-string (pcn-form i))))

(deftest pprint-print-object-circle-labels.shape-1
  (mapcar (lambda (m) (pcn-print 1 m)) '(10 14 16 20 24 30))
  ("(#1=#S<#2=(1
           2
           3)>
 #1# #2#
 #2#)" "(#1=#S<#2=(1
           2
           3)>
 #1# #2# #2#)" "(#1=#S<#2=(1 2
           3)>
 #1# #2# #2#)" "(#1=#S<#2=(1 2 3)>
 #1# #2# #2#)" "(#1=#S<#2=(1 2 3)> #1#
 #2# #2#)" "(#1=#S<#2=(1 2 3)> #1# #2#
 #2#)"))

(deftest pprint-print-object-circle-labels.shape-2
  (mapcar (lambda (m) (pcn-print 2 m)) '(10 14 16 20 24 30))
  ("(#1=#<CNT 9>
 #1#
 (#1#))" "(#1=#<CNT 9>
 #1# (#1#))" "(#1=#<CNT 9>
 #1# (#1#))" "(#1=#<CNT 9> #1#
 (#1#))" "(#1=#<CNT 9> #1# (#1#))" "(#1=#<CNT 9> #1# (#1#))"))

(deftest pprint-print-object-circle-labels.shape-3
  (mapcar (lambda (m) (pcn-print 3 m)) '(10 14 16 20 24 30))
  ("((#1=#<CNT 9>)
 #1# #1#
 #1# #1#)" "((#1=#<CNT 9>)
 #1# #1# #1#
 #1#)" "((#1=#<CNT 9>)
 #1# #1# #1#
 #1#)" "((#1=#<CNT 9>) #1#
 #1# #1# #1#)" "((#1=#<CNT 9>) #1# #1#
 #1# #1#)" "((#1=#<CNT 9>) #1# #1# #1#
 #1#)"))

(deftest pprint-print-object-circle-labels.shape-4
  (mapcar (lambda (m) (pcn-print 4 m)) '(10 14 16 20 24 30))
  ("(#1=#S<#2=(1
           2
           3)>
 #S<#3=(4
        5)>
 #3# #1#
 #2# #3#)" "(#1=#S<#2=(1
           2
           3)>
 #S<#3=(4 5)>
 #3# #1# #2#
 #3#)" "(#1=#S<#2=(1 2
           3)>
 #S<#3=(4 5)>
 #3# #1# #2#
 #3#)" "(#1=#S<#2=(1 2 3)>
 #S<#3=(4 5)> #3#
 #1# #2# #3#)" "(#1=#S<#2=(1 2 3)>
 #S<#3=(4 5)> #3# #1#
 #2# #3#)" "(#1=#S<#2=(1 2 3)>
 #S<#3=(4 5)> #3# #1# #2# #3#)"))

(deftest pprint-print-object-circle-labels.shape-5
  (mapcar (lambda (m) (pcn-print 5 m)) '(10 14 16 20 24 30))
  ("(#1=(1 2
     3)
 (#2=#S<#1#>)
 #2#
 (#1#))" "(#1=(1 2 3)
 (#2=#S<#1#>)
 #2# (#1#))" "(#1=(1 2 3)
 (#2=#S<#1#>)
 #2# (#1#))" "(#1=(1 2 3)
 (#2=#S<#1#>) #2#
 (#1#))" "(#1=(1 2 3)
 (#2=#S<#1#>) #2# (#1#))" "(#1=(1 2 3) (#2=#S<#1#>) #2#
 (#1#))"))

(deftest pprint-print-object-circle-labels.shape-6
  (mapcar (lambda (m) (pcn-print 6 m)) '(10 14 16 20 24 30))
  ("(#1=#S<#2=(1
           2
           3)>
 (#1#
  #1#)
 (#3=#<CNT 9>
  (#3#))
 #3# #2#)" "(#1=#S<#2=(1
           2
           3)>
 (#1# #1#)
 (#3=#<CNT 9>
  (#3#))
 #3# #2#)" "(#1=#S<#2=(1 2
           3)>
 (#1# #1#)
 (#3=#<CNT 9>
  (#3#))
 #3# #2#)" "(#1=#S<#2=(1 2 3)>
 (#1# #1#)
 (#3=#<CNT 9>
  (#3#))
 #3# #2#)" "(#1=#S<#2=(1 2 3)>
 (#1# #1#)
 (#3=#<CNT 9> (#3#))
 #3# #2#)" "(#1=#S<#2=(1 2 3)> (#1# #1#)
 (#3=#<CNT 9> (#3#)) #3# #2#)"))

(deftest pprint-print-object-circle-labels.shape-7
  (mapcar (lambda (m) (pcn-print 7 m)) '(10 16 20 24 30))
  ("(0
 #1=#S<(1
        2
        3)>
 #2=#<CNT 9>
 #1# #2#)" "(0
 #1=#S<(1 2 3)>
 #2=#<CNT 9>
 #1# #2#)" "(0 #1=#S<(1 2 3)>
 #2=#<CNT 9> #1#
 #2#)" "(0 #1=#S<(1 2 3)>
 #2=#<CNT 9> #1# #2#)" "(0 #1=#S<(1 2 3)> #2=#<CNT 9>
 #1# #2#)"))
