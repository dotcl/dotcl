# dotcl-socket

TCP sockets as Lisp streams: `make-server-socket` (bind and listen),
`socket-accept`, `socket-connect`, `local-port` (which is how you find out what
port 0 became), `socket-stream` and `socket-close`. Accepting or connecting
gives back a bidirectional stream, so the ordinary stream functions work on it.

    (require "dotcl-socket")

A small layer over the `dotcl-bsd-sockets` contrib. For UDP, socket options
and timeouts, use a portable library such as usocket.
