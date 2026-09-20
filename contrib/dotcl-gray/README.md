# dotcl-gray

Gray streams: the `dotcl-gray:fundamental-*` class hierarchy and the generic
functions that go with it (`stream-read-char`, `stream-write-char`,
`stream-read-byte`, `stream-line-column`, and the rest of the protocol).

    (require "dotcl-gray")

Define a CLOS class inheriting from one of the fundamental stream classes and
add methods to those generic functions; the runtime then dispatches the ordinary
CL stream functions to them whenever such an instance is used as a stream. No
dependencies.
