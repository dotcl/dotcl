# dotcl-socket

TCP sockets: `make-server-socket` (bind and listen), `socket-accept`,
`socket-connect`, `local-port` (which is how you find out what port 0 became),
`socket-stream` and `socket-close`. Accepting or connecting gives back a
bidirectional stream, so the ordinary stream functions work on it.

    (require "dotcl-socket")

A thin layer over System.Net.Sockets, with no dependencies.
