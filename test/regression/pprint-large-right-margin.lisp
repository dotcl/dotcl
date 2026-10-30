;;; A right margin at or above 2^31 used to wrap every element onto its own
;;; line because it was truncated to an int. SBCL 2.6.8 keeps these forms on
;;; one line for every margin larger than their width; a bignum margin is
;;; accepted here and clamped instead of wrapping.

(defun %plrm-print (object margin)
  (let ((*print-right-margin* margin)
        (*print-pretty* t)
        (*print-escape* t)
        (*print-readably* nil)
        (*print-length* nil)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-circle* nil)
        (*print-miser-width* nil)
        (*print-case* :upcase))
    (prin1-to-string object)))

(defun %plrm-eval (mode margin object)
  (let ((dotcl:*evaluator-mode* mode))
    (eval `(let ((*print-right-margin* ,margin)
                 (*print-pretty* t)
                 (*print-escape* t)
                 (*print-readably* nil)
                 (*print-length* nil)
                 (*print-level* nil)
                 (*print-lines* nil)
                 (*print-circle* nil)
                 (*print-miser-width* nil)
                 (*print-case* :upcase))
             (prin1-to-string ',object)))))

(defparameter %plrm-margins
  (list (1- (expt 2 31)) (expt 2 31) (expt 2 40)
        most-positive-fixnum (expt 10 100)))

(defparameter %plrm-long-form
  '(alpha beta gamma delta epsilon zeta eta theta iota kappa lambda
    mu nu xi omicron pi rho sigma tau upsilon))

(defparameter %plrm-long-line
  "(ALPHA BETA GAMMA DELTA EPSILON ZETA ETA THETA IOTA KAPPA LAMBDA MU NU XI OMICRON PI RHO SIGMA TAU UPSILON)")

(deftest pprint-large-right-margin.small-form
  (mapcar (lambda (margin) (%plrm-print '(= 3 (add 1 1)) margin))
          %plrm-margins)
  ("(= 3 (ADD 1 1))" "(= 3 (ADD 1 1))" "(= 3 (ADD 1 1))"
   "(= 3 (ADD 1 1))" "(= 3 (ADD 1 1))"))

(deftest pprint-large-right-margin.long-form
  (mapcar (lambda (margin) (%plrm-print %plrm-long-form margin))
          %plrm-margins)
  (#.%plrm-long-line #.%plrm-long-line #.%plrm-long-line
   #.%plrm-long-line #.%plrm-long-line))

(deftest pprint-large-right-margin.interpreted
  (mapcar (lambda (margin) (%plrm-eval :interpret margin %plrm-long-form))
          %plrm-margins)
  (#.%plrm-long-line #.%plrm-long-line #.%plrm-long-line
   #.%plrm-long-line #.%plrm-long-line))
