;; A struct through cl-autowrap, then the C library itself.
(let ((r (sdl2:make-rect 1 2 3 4)))
  (setf (sdl2:rect-x r) 10)
  (smoke "rect" (list (sdl2:rect-x r) (sdl2:rect-y r) (sdl2:rect-width r) (sdl2:rect-height r))))
(sdl2:init :timer)
(smoke "version" (multiple-value-list (sdl2:version)))
(smoke "ticks" (integerp (sdl2:get-ticks)))
(sdl2:quit)
