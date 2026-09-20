# dotcl-lsp-api

What an editor asks a live image about the code at a point:
`dotcl-lsp-api:completions` (candidates and the range to replace),
`documentation-url` (the reference page for the name under the cursor) and
`describe-at` (what that name is -- kind, signatures, documentation).
Completion covers Lisp symbols and .NET type and member names.

    (require "dotcl-lsp-api")

Each call takes the text and an offset into it, because that is what every
caller already has: a REPL holds the line up to point, an editor holds the
document and a position. No protocol lives here -- the wire belongs to whatever
speaks it -- so this is equally callable from the bundled REPL, from a swank
function, or from a language server in another image. No dependencies.
