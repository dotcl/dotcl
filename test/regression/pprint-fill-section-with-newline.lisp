;;; A fill newline breaks when the section after it cannot be printed on the
;;; end of the line, and a section that holds a mandatory newline (here from a
;;; nested block) never can.

(defclass pfsn-node () ((tag :initarg :tag) (children :initarg :children)))

(defmethod print-object ((n pfsn-node) s)
  (format s (if (cdr (slot-value n 'children))
                "~@<<~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                "~@<<~a>~2I~:_~<~a~:>~0I~:_</~a>~:>")
          (slot-value n 'tag) (slot-value n 'children) (slot-value n 'tag)))

(defun pfsn-node (tag &rest children)
  (make-instance 'pfsn-node :tag tag :children children))

(deftest pprint-fill-section-with-newline.nested-block-with-mandatory-newline
  (with-output-to-string (s)
    (write (pfsn-node "cat" (pfsn-node "dog" "img" "woo")) :stream s))
  "<cat>
  <dog>
    img
    woo
  </dog>
</cat>")

;; The neighbours that must not change: a short child stays on the line, and a
;; fill newline whose section fits still does not break.
(deftest pprint-fill-section-with-newline.short-child-stays-on-the-line
  (with-output-to-string (s)
    (write (pfsn-node "cat" (pfsn-node "dog" "img")) :stream s))
  "<cat><dog>img</dog></cat>")

(deftest pprint-fill-section-with-newline.fill-fits
  (with-output-to-string (s)
    (pprint-logical-block (s nil)
      (format s "ab~:_cd~:@_ef")))
  "abcd
ef")
