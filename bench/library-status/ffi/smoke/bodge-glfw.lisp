;; bodge-glfw does not load the library itself; glfw-blob carries it.
(ql:quickload "glfw-blob" :silent t)
;; glfwGetVersion writes through three int pointers.
(cffi:with-foreign-objects ((major :int) (minor :int) (rev :int))
  (funcall (find-symbol "GET-VERSION" "%GLFW") major minor rev)
  (smoke "version" (list (cffi:mem-ref major :int) (cffi:mem-ref minor :int)
                         (cffi:mem-ref rev :int))))
