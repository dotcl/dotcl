;;; A first clause ended by ~:; is discarded when the justified text fits
;;; (CLHS 22.3.6.2). Expected value is SBCL 2.6.8's output.

(deftest format-justification-overflow-first-clause.fits
  (let ((*print-pretty* t) (*print-right-margin* 30))
    (format nil "~20<~a~:;~a~;~a~>" 'alpha 'beta 'gamma))
  "BETA           GAMMA")
