# dotcl-bsd-sockets

TCP and UDP sockets over System.Net.Sockets, in the shape of SBCL's
sb-bsd-sockets. This is the layer under the dotcl backends of socket libraries
such as usocket, not something to use directly; programs should use those
libraries. The shape follows sb-bsd-sockets so that a library's SBCL code path
is a good guide to what its dotcl backend calls:

- socket objects: `inet-socket`, `inet6-socket` (`:type :stream` or
  `:datagram`), with `socket-bind`, `socket-listen`, `socket-accept`,
  `socket-connect` (with `:timeout`), `socket-close`, `socket-shutdown`,
  `socket-name`, `socket-peername`, `socket-make-stream`, and `socket-send` /
  `socket-receive` for datagrams
- options: `sockopt-reuse-address`, `sockopt-keep-alive`, `sockopt-tcp-nodelay`,
  `sockopt-broadcast`, `sockopt-receive-timeout`, `sockopt-send-timeout`
  (seconds; setf-able)
- names: `get-host-by-name`, `get-host-by-address` (a `host-ent`),
  `get-host-name`. IP literals, including `"0.0.0.0"` and `"::"`, are never
  sent to the resolver
- one call for the usual jobs: `tcp-connect`, `tcp-listen`, `udp-open`, and
  `wait-for-input` over several sockets
- conditions: `socket-error` and a subclass per kind (`connection-refused-error`,
  `operation-timeout-error`, `address-in-use-error`, ...), `name-service-error`
  and `host-not-found-error`. `socket-error-from-exception` translates a .NET
  exception raised elsewhere, such as by a read on a socket stream

```lisp
(require "dotcl-bsd-sockets")
(let* ((server (dotcl-bsd-sockets:tcp-listen "0.0.0.0" 0))
       (port (nth-value 1 (dotcl-bsd-sockets:socket-name server)))
       (client (dotcl-bsd-sockets:tcp-connect "127.0.0.1" port :timeout 5)))
  (dotcl-bsd-sockets:socket-accept server))
```

Addresses come back as octet vectors (4 octets for IPv4, 16 for IPv6).
