;;; SETF of a DOTNET:REF place returns the stored value (CLHS 5.1.1).
;;;
;;; Its defsetf expanded to the void set_Item call, so the element was stored
;;; but the setf form yielded NIL. The value form must still run only once.

(require "dotnet-class")

(defun drsv-list (&rest items)
  (let ((l (dotnet:new (dotnet:make-generic-type
                        "System.Collections.Generic.List"
                        (list "System.Int32")))))
    (dolist (item items l) (dotnet:invoke l "Add" item))))

(deftest drsv-setf-returns-value
  (let* ((l (drsv-list 10 20))
         (returned (multiple-value-list (setf (dotnet:ref l 1) 99))))
    (list returned (dotnet:ref l 1)))
  ((99) 99))

(deftest drsv-value-form-evaluated-once
  (let* ((l (drsv-list 1))
         (calls 0)
         (returned (multiple-value-list
                    (setf (dotnet:ref l 0) (progn (incf calls) 42)))))
    (list returned calls (dotnet:ref l 0)))
  ((42) 1 42))
