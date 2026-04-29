;;;; WinForms click counter demo
;;;;
;;;; (windows-examples:winforms-demo)

(in-package :windows-examples)

(defun winforms-demo ()
  "Open a WinForms window with a click counter.
Blocks until the window is closed."
  (dotnet:load-assembly "System.Windows.Forms")
  (let ((count 0))
    (dotnet:ui-invoke
      (lambda ()
        (let* ((form   (dotnet:new "System.Windows.Forms.Form"))
               (label  (dotnet:new "System.Windows.Forms.Label"))
               (button (dotnet:new "System.Windows.Forms.Button")))

          (dotnet:invoke form   "set_Text"   "dotcl WinForms demo")
          (dotnet:invoke form   "set_Width"  300)
          (dotnet:invoke form   "set_Height" 160)

          (dotnet:invoke label  "set_Text"   "Click count: 0")
          (dotnet:invoke label  "set_Left"   20)
          (dotnet:invoke label  "set_Top"    20)
          (dotnet:invoke label  "set_Width"  240)
          (dotnet:invoke label  "set_Height" 30)

          (dotnet:invoke button "set_Text"   "Click me!")
          (dotnet:invoke button "set_Left"   20)
          (dotnet:invoke button "set_Top"    60)
          (dotnet:invoke button "set_Width"  120)

          (dotnet:add-event button "Click"
            (lambda (sender e)
              (declare (ignore sender e))
              (incf count)
              (dotnet:invoke label "set_Text"
                             (format nil "Click count: ~a" count))
              (dotnet:invoke form "set_Text"
                             (format nil "dotcl WinForms demo (~a)" count))))

          (let ((controls (dotnet:invoke form "get_Controls")))
            (dotnet:invoke controls "Add" label)
            (dotnet:invoke controls "Add" button))

          (dotnet:invoke form "ShowDialog"))))))
