# McpServerDemo — dotcl as an MCP tool

A minimal sample that exposes dotcl (Common Lisp on .NET) as a **Model Context
Protocol** server. When Claude Desktop / Cursor / any MCP client calls
`lisp_eval` as a tool, the Lisp form is evaluated in the dotcl image inside this
process and the result string is returned to the LLM.

The goal is to make dotcl's eval / define-class / interop a "tool for the LLM to
think with", directly.

## Exposed tool

- **`lisp_eval(code)`** — takes Common Lisp source and returns
  `(prin1-to-string (progn <code>))`. Side effects such as DEFUN / DEFVAR are
  session-persistent (they live as long as the server process does).

## Build

```
dotnet build samples/McpServerDemo/McpServerDemo.csproj -c Release
```

Output: `bin/Release/net10.0/McpServerDemo.exe` (with `dotcl.core` and the
`contrib/` set bundled into the output directory).

## Register with Claude Desktop

Add the following to `%APPDATA%\Claude\claude_desktop_config.json` (Windows):

```json
{
  "mcpServers": {
    "dotcl": {
      "command": "C:\\path\\to\\dotcl-a\\samples\\McpServerDemo\\bin\\Release\\net10.0\\McpServerDemo.exe"
    }
  }
}
```

Restart Claude Desktop and the `dotcl` provider appears in the tool list, with
`lisp_eval` visible.

## For Cursor

Cursor's MCP settings (the settings UI or `~/.cursor/mcp.json`) accept the same
format.

## Try it

In a Claude Desktop conversation:

> Evaluate (+ 1 2 3 4 5) with dotcl

→ Claude calls `lisp_eval` and `"15"` comes back.

DEFUN works too:

> Evaluate (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))),
> then evaluate (fact 10)

→ the first call defines fact, the second returns `"3628800"`.

## Implementation notes

- The dotcl runtime is not thread-safe, so eval is serialized with `_evalLock`.
- `DotclHost.LoadCore` boots only once (it takes ~0.3s).
- The MCP protocol owns stdout, so all logs go to stderr (`LogToStandardErrorThreshold=Trace`).
- Multiple forms are wrapped in PROGN, so **only the last value** is returned.
- Errors are returned as a string of the form `ERROR (<ExceptionType>): <message>`
  (so the LLM can recover naturally instead of the server crashing).

## See also

- [ModelContextProtocol C# SDK](https://github.com/modelcontextprotocol/csharp-sdk) (1.2.0, stable)
- [MCP spec](https://modelcontextprotocol.io/)
