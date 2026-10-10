(smoke "init timer" (sdl-cffi::sdl-init sdl-cffi::sdl-init-timer))
(smoke "ticks" (integerp (sdl-cffi::sdl-get-ticks)))
(sdl-cffi::sdl-quit)
