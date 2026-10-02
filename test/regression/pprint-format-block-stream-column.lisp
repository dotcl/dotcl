;;; A FORMAT logical block that starts after text already on the line of the
;;; destination string stream (written before the FORMAT call) begins at that
;;; column, so its continuation lines are indented to it. The expected strings
;;; are SBCL 2.6.8's. The block's start column used to be taken as 0.

(defun pfbsc-print (lead)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* 40))
      (write-string lead s)
      (format s "~@<aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk~:@>"))))

(deftest pprint-format-block-stream-column.after-text
  (pfbsc-print "- ")
  "- aaaa bbbb cccc dddd eeee ffff gggg
  hhhh iiii jjjj kkkk")

(deftest pprint-format-block-stream-column.after-newline
  (pfbsc-print (format nil "x~%- "))
  "x
- aaaa bbbb cccc dddd eeee ffff gggg
  hhhh iiii jjjj kkkk")
