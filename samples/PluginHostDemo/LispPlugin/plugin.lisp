;;; The Lisp plugin: a .NET library whose class implements the host's IPlugin,
;;; with the method bodies compiled into the .dll. LispPlugin.csproj builds it;
;;; the host's contract assembly is a ProjectReference there, so IPlugin is
;;; already loaded when this file is.

(defpackage :lisp-plugin (:use :cl))
(in-package :lisp-plugin)

(dotnet:library ("LispPlugin" :version "1.0.0.0")
  (:defun split-words (s)
    (let ((words '()) (start nil))
      (dotimes (i (length s))
        (if (char= (char s i) #\Space)
            (when start (push (subseq s start i) words) (setf start nil))
            (unless start (setf start i))))
      (when start (push (subseq s start) words))
      (nreverse words)))
  (:class "LispPlugin.ReversePlugin" ()
    (:implements "PluginHost.IPlugin")
    (:properties ("Name" String))
    (:events ("Completed" "System.Action`1[System.String]"))
    (:ctor ()
      (dotnet:%set-invoke self "Name" "reverse words (Common Lisp)"))
    (:methods
      (run ((input String) (progress "System.Action`1[System.String]")) :returns String
        (let* ((words (split-words input))
               (result (format nil "~{~A~^ ~} [~D words]"
                               (mapcar #'reverse words) (length words))))
          (dolist (w words) (funcall progress w))
          (dotnet:invoke self "OnCompleted" result)
          result)))))
