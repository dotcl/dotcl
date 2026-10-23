# dotcl-jsonrpc

JSON and the framing the Debug Adapter Protocol and the Language Server
Protocol share: a `Content-Length` header, a blank line, then that many bytes
of UTF-8 JSON. Used by `dotcl-dap` and `dotcl-lsp`.

    (require "dotcl-jsonrpc")
    (dotcl-jsonrpc:write-message stream '(:obj "jsonrpc" "2.0" "id" 1 "result" nil))
    (dotcl-jsonrpc:read-message stream)   ; NIL at end of input

`stream` is a binary stream. A JSON object is `(:obj "key" value ...)`, an
array a vector, true `T`, false `:FALSE`, null `NIL`; `jget` reads a key of
an object. `json-encode` and `json-decode` convert to and from a string.
