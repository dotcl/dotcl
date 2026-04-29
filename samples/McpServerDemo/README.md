# McpServerDemo — dotcl as an MCP tool

dotcl (Common Lisp on .NET) を **Model Context Protocol** サーバとして
公開する最小サンプル。Claude Desktop / Cursor / その他 MCP client から
`lisp_eval` を tool として呼ぶと、このプロセス内の dotcl image で
Lisp form が評価され、結果文字列が LLM に返る。

dotcl の eval / define-class / interop がそのまま LLM の"思考の道具"
になる絵作りが狙い。

## 公開される tool

- **`lisp_eval(code)`** — Common Lisp ソースを受け取り、
  `(prin1-to-string (progn <code>))` を返す。DEFUN / DEFVAR などの
  副作用は session-persistent (サーバプロセスが生きている間は残る)。

## ビルド

```
dotnet build samples/McpServerDemo/McpServerDemo.csproj -c Release
```

生成物: `bin/Release/net10.0/McpServerDemo.exe` (and `dotcl.core`,
`contrib/` 一式が出力ディレクトリに bundle される)。

## Claude Desktop に登録

`%APPDATA%\Claude\claude_desktop_config.json` (Windows) に以下を追加:

```json
{
  "mcpServers": {
    "dotcl": {
      "command": "C:\\path\\to\\dotcl-a\\samples\\McpServerDemo\\bin\\Release\\net10.0\\McpServerDemo.exe"
    }
  }
}
```

Claude Desktop を再起動すると、ツール一覧に `dotcl` provider が現れ、
`lisp_eval` が見える。

## Cursor の場合

Cursor の MCP 設定 (設定画面または `~/.cursor/mcp.json`) にも同じ
形式で登録可能。

## 動作確認

Claude Desktop の会話で:

> dotcl で (+ 1 2 3 4 5) を評価して

→ Claude が `lisp_eval` を呼び、`"15"` が返る。

DEFUN なども:

> (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) を評価して、
> そのあと (fact 10) を評価して

→ 初回 call で defun、2 回目 call で `"3628800"`。

## 実装ノート

- dotcl runtime は thread-safe でないので、`_evalLock` で eval を直列化
- `DotclHost.LoadCore` は boot 1 回だけ (0.3s ほどかかる)
- MCP プロトコルが stdout を占有するので、log は全部 stderr (`LogToStandardErrorThreshold=Trace`)
- 複数 form を入力すると PROGN で包まれて **最後の値だけ** 返す
- エラーは `ERROR (<ExceptionType>): <message>` 形式の文字列で返す
  (crash させずに LLM が自然に recover できるように)

## 関連

- [ModelContextProtocol C# SDK](https://github.com/modelcontextprotocol/csharp-sdk) (1.2.0, stable)
- [MCP spec](https://modelcontextprotocol.io/)
- dotcl issue #163: `Demo: MCP server で dotcl REPL を LLM の tool にする`
