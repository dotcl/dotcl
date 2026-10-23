# dotcl-lsp

A Language Server Protocol server inside the running process. An editor
starts it as a child process and talks to it on standard input and output:

    dotcl --eval '(require "dotcl-lsp")' --eval '(dotcl:quit (dotcl-lsp:serve-stdio))'

or a client connects over TCP to one started from a REPL:

    (require "dotcl-lsp")
    (dotcl-lsp:start-server :port 0)      ; :port 0 picks a free one
    (dotcl-lsp:server-port dotcl-lsp:*server*)

While `serve-stdio` runs, what Lisp code prints goes to standard error.

Messages answered: `initialize`, `initialized`, `shutdown`, `exit`,
`textDocument/didOpen`, `didChange` (whole or incremental), `didSave`,
`didClose`, and `textDocument/hover`. After each open and change the server
sends `textDocument/publishDiagnostics`. Documents are kept as the editor sends
them; nothing is read from disk, loaded or evaluated.

What is reported comes from the functions on `dotcl-lsp:*diagnostic-functions*`
(a document to a list of diagnostics with character offsets) and
`dotcl-lsp:*hover-functions*` (a document and an offset to Markdown). The
server converts offsets to LSP positions: lines, and columns in UTF-16 code
units. On its own it reports no diagnostics, and hover describes the Lisp name
under the cursor (`dotcl-code-info:describe-at`). `dotcl-lsp-coalton` adds
Coalton.

One client per connection, one connection at a time on TCP.
