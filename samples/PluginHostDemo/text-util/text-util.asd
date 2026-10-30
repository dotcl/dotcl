;;; A plain Common Lisp library with no .NET in it. WordStatsPlugin depends on
;;; it, and the functions of it the plugin calls are compiled into the plugin's DLL.
(defsystem "text-util"
  :components ((:file "text-util")))
