# dotcl - Common Lisp on .NET

.NET 上で動く Common Lisp 処理系。Lisp ソースを CIL (Common
Intermediate Language) にコンパイルし .NET の JIT で実行する。
ANSI 規格適合は ansi-test (gitlab.common-lisp.net/ansi-test/ansi-test)
で 21,928/21,929 (99.99%) pass。

このドキュメントは dotcl の **現状の実装** を機構ごとに記述する。
時系列の作業ログは CHANGELOG.md、個別の設計判断は
`docs/decisions/D{NNN}-...md` に分かれている。本ドキュメントは
両者の中間 — 「いま何がどう動いているのか」を機構別にまとめる。

## 1. 背景

SBCL ARM64 Windows ポート (2026-02 upstream 取り込み) のあと、
新規プラットフォーム対応のたびにレジスタ割付・呼び出し規約・GC
バックエンドを書き起こす SBCL 流のコストを別の方向で回避できないか
という問いが出発点。managed runtime に載せる方針を採るとして、
なぜ .NET なのか (§1.5)、その中でなぜ .NET 10 をターゲットにしたか
(§1.6) を順に置く。

## 1.5 なぜ .NET が Common Lisp ランタイムとして適しているか

Common Lisp を managed runtime に載せると決めたとき、ランタイムに
要求するものは概ね固定されている — GC、スレッド、構造化例外
(= コンディション)、実行時コード生成 (= eval / compile)、ホスト
型システムとの相互運用、プラットフォーム横断性。.NET はこれら
すべてに対し、Microsoft が full-time で保守する first-party 実装を
提供している。「自前で書かなくていい」を超えて、「CL 固有の意味論を
ホスト機構に直接写像できる」という点が大きい。

### GC・スレッド・I/O

generational GC + `WeakReference` で CL の GC 要件 (循環参照の安全な
回収、weak hash table) がそのまま満たせる。スレッドは `Thread` /
`Task` / `Monitor` / `Interlocked` までセットで揃っていて、
bordeaux-threads 互換 API を 414 行で書ける (§3.14)。SBCL は同等の
ものを GC バックエンド・OS API ラッパー・thread tunable まで全部
自前で抱えている。

### 構造化例外 → コンディション

CL の handler-bind / handler-case は .NET の `try/catch` + filter
(`catch (...) when (...)`) に自然に写像できる。handler-bind が
unwind せず handler を呼ぶ semantics は exception filter で、
handler-case の unwind 型は通常の try/catch で表現できる。CL の
condition オブジェクトを `Exception` のサブクラスにすれば、Lisp 側
で signal した condition を C# 側の `catch (LispCondition)` で
そのまま受けられる (§3.11)。JVM のチェック例外と非チェック例外の
混在 + 二段階 filter の不在に比べると素直。

### 実行時コード生成 (Reflection.Emit)

CL は eval / compile / defun-at-runtime / クラス再定義など「実行中に
コードを生成する」が日常の言語。.NET は `System.Reflection.Emit`
(`DynamicMethod`, `ILGenerator`, `PersistedAssemblyBuilder`) を
first-party API として提供しており、これが dotcl の compiler
バックエンド (§3.7-3.8) のすべて。JVM では ASM や ByteBuddy などの
外部ライブラリを噛ませる必要があり、API の安定性と保守責任の階層が
一段下がる。

### ホスト型システムとの interop

`dotnet:define-class` で emit した CLR クラスは Reflection から
ただの .NET 型として見える (§3.16)。MAUI の data binding / ASP.NET
Core の routing / JSON シリアライザの auto-discovery が、Lisp で
定義したクラスに対しても素のまま効く。JVM で同等のことをやるには
ByteBuddy / cglib で実行時バイトコードを emit して class loader に
流し込む必要があり、library 依存と class loader hierarchy の問題が
追加で乗ってくる。

### プラットフォーム横断性

ランタイムが Windows / macOS / Linux × x86-64 / ARM64 で標準提供
される。FASL は IL only なので、Windows でビルドした `.fasl` を
Linux で `(load ...)` してもそのまま動く。SBCL の per-arch ポート
作業 (レジスタ・呼び出し規約・GC バックエンド) は不要。

### NativeAOT publish

.NET 8+ の NativeAOT は IL アプリを単一のネイティブ実行バイナリ
(Windows / Linux / macOS × x86-64 / ARM64) として publish できる —
JIT / ランタイム依存なしで起動する成果物を作れる点は JVM 系には
基本的にない強み。dotcl 本体は Reflection.Emit に依存するため
fully AOT 化は今のところできないが、eval / load を切った
precompiled `.fasl` だけで動くアプリ形態なら AOT publish に
乗せる余地があり、.NET を選ぶ理由として将来の伸びしろを担保する。

### NuGet エコシステム

HTTP/2 / gRPC / JSON / ML.NET / Entity Framework といった現代の
業界標準を Lisp から `dotnet:require` で直接 load できる。Lisp 側で
無理に書き直さなくても、エコシステム全体を借りる前提で設計できる。

### 性能

型宣言を効かせた数値計算で SBCL の 1〜1.5 倍程度。実用範囲。

## 1.6 なぜ .NET 10 か

.NET の中でなぜ 10 をターゲットにしたか — `PersistedAssemblyBuilder`
の安定化が直接の理由。

- **`PersistedAssemblyBuilder` の安定化**: .NET Framework に存在した
  `AssemblyBuilder.Save` は .NET Core で削除されていた。.NET 9 で
  `System.Reflection.Emit.PersistedAssemblyBuilder` として復活し、
  .NET 10 で安定化。これにより compile-file が `.fasl` (PE assembly)
  をディスクに書き出せるようになり、A2 方式 (§2) の静的経路が成立
  する。.NET 10 未満では同じアーキテクチャを取れない。
- **`Reflection.Emit` 経路**: 動的経路 (eval / load) は引き続き
  `DynamicMethod` + `ILGenerator`。.NET 10 でも core API として
  維持されている。
- **ReadyToRun**: 生成済み `.fasl` を含むアプリの起動を R2R で
  高速化できる経路が .NET 8/9/10 と段階的に整備されてきた。
- **Span / Rune / ConcurrentDictionary**: .NET 5+ で揃った高速な
  primitive を Reader / コンディションスタック / パッケージシステム
  がそのまま使う。

## 2. 全体像 (A2 方式)

```
Source (.lisp)
    │
    ▼
  Reader (S 式パーサ) ← C#
    │
    ▼
  CIL コンパイラ (Lisp 実装)
    │ S 式を受け取り、CIL 命令リスト (S 式データ) を返す
    ▼
  命令リスト  ((:ldc-i8 1) (:call "Fixnum.Make") ...)
    │
    ├─→  C# Assembler ─→ DynamicMethod         (eval / load)
    └─→  C# Assembler ─→ PersistedAssemblyBuilder ─→ .fasl
                                                    (compile-file)
    │
    ▼
  .NET CLR (JIT → ネイティブ実行)
```

**コンパイラは純粋関数**: S 式を受け取り、命令リストを返す。副作用
なし、.NET API 呼び出しなし。`.NET API` を叩くのは C# 側のアセンブラ
だけ。コンパイラが Lisp で書かれているので eval / load / compile-file
が同一コードを通り、セルフホストが成立している
(`DOTCL_LISP=dotcl make cross-compile` で SBCL なしで再ビルド可能)。

命令リストはデータなので、デバッグ時に print して中身を確認できる。
最適化パスを後段に挿入することも、別バックエンド (例: WASM) に差し
替えることも、原理的には命令リストの変換器として書ける。

## 3. 機構別実装ノート

dotcl が ANSI Common Lisp として最低限抱えなければならない機構を、
データ表現 → 主要ファイル → 実装上の論点 → ANSI / SBCL との差分の
4 点で記述する。

### 3.1 数値塔

**データ表現**: `Number` (抽象基底) → `Fixnum` (`long`、−128〜65535
をキャッシュ) / `Bignum` (`System.Numerics.BigInteger`) / `Ratio`
(分子・分母とも `BigInteger`、GCD で自動約分) / `SingleFloat` /
`DoubleFloat` / `LispComplex` (実部・虚部とも `Number`)。

**主要なファイル**: `runtime/Numbers.cs` (型階層と Fixnum/LispChar
キャッシュ)、`runtime/Runtime.Arithmetic.cs` (演算ディスパッチと
contagion)、`runtime/Runtime.Predicates.cs` (`numberp` / `floatp`
等の型述語)。

**実装上の論点**: `Fixnum` の `+ - *` はインライン化された fast path
で sign-bit XOR による overflow 検出を行い、桁あふれ時に `Bignum` へ
昇格する。Float は `ToString("R")` で round-trip 再現できる文字列を
出力し、`SingleFloat` は `1.0f0` / `DoubleFloat` は `1.0d0` のように
読み戻し可能な指数マーカを出す。`Ratio.Make` は分母 1 で `Fixnum` に
自動降格。

**ANSI / SBCL との差分**: 数値塔は ANSI 準拠。SBCL のような unboxed
fixnum 表現は持たず、すべての数値はヒープオブジェクト。型宣言を
効かせて `Fixnum.Value` を直接演算する最適化は将来課題 (Step 7 の
範疇)。

### 3.2 シンボルとパッケージ

**データ表現**: `Symbol` は `Name` (string) と `HomePackage` のほか、
`Value` / `Function` / `SetfFunction` を **volatile** フィールドで
保持 (volatile によりクロススレッド可視性を確保し、書き込み毎の
ロックを避ける)。`Plist` は通常の参照型。`Package` は
`ConcurrentDictionary<string, Symbol>` を internal / external に
1 つずつ持ち、use-list は `List<Package>`、複数操作の atomic 化に
`_pkgLock` を使う。

**主要なファイル**: `runtime/Symbol.cs`、`runtime/Package.cs`、
`runtime/Runtime.Packages.cs` (find-package / use-package /
do-external-symbols 系の API)。

**実装上の論点**: `find-symbol` は external → internal → 継承
(use-list 走査) の順で検索。`KEYWORD` パッケージは intern 時に自動
export され、シンボルは self-evaluating。CDR 5
package-local-nicknames を `_localNicknames` で実装。volatile
フィールドの選択は #171 に対応するスレッドセーフ化の第 1 段で、
個別の per-symbol ロックは将来必要になったら入れる。

**ANSI / SBCL との差分**: `setf` 関数を専用レジストリではなくシンボル
直属の `SetfFunction` フィールドに置く ((setf foo) を高速に解決する
ための選択、#58 Phase 1)。それ以外は ANSI 通り。

### 3.3 Reader

**データ表現**: `Reader` は `TextReader` をラップし、push-back バッファ
と行番号トラッキング、`#n=` / `#n#` 共有ラベルテーブルを保持する。
`LispReadtable` は文字 → ディスパッチャ (`Func<Reader, char, int,
LispObject?>`) のテーブルで、`#` で始まるディスパッチ系も同じ仕組み。

**主要なファイル**: `runtime/Reader.cs` (1985 行、CLHS 2.2 の token
組み立て手順をそのまま実装)、`runtime/Readtable.cs` (`SyntaxType`、
`ReadtableCase`、ディスパッチテーブル管理)。

**実装上の論点**: パッケージ修飾子 (`foo:bar` / `foo::bar`) は token
を一度組み立てた後で escape されていないコロン位置を探して解析する。
バッククォート展開は **read 時** に行われ (D001)、結果には quasiquote
/ unquote シンボルが残らない。`#+` / `#-` は dotcl 独自の feature
expression 評価器を read 時に走らせる。`#` ディスパッチ
(`#\` / `#'` / `#(` / `#A` / `#B` / `#O` / `#X` / `#:` / `#=` / `##` /
`#.` 等) は C# 側のラムダで実装し、ユーザ定義の dispatch macro は
Lisp 関数を C# ラッパで包む。

**ANSI / SBCL との差分**: backquote が read 時展開なため、`(quasiquote
...)` を保持してマクロから検査する用途には使えない (実装の単純化と
速度のための選択)。それ以外は CLHS 通り。

### 3.4 Printer / フォーマッタ

**データ表現**: `Runtime.Printer` が `write` / `print` / `princ` /
`prin1` を提供し、`*print-case*` / `*print-readably*` / `*print-gensym*`
/ `*print-circle*` 等のダイナミック変数で挙動を切り替える。
`Runtime.Format` は `format` 制御文字列をパースし `~A` / `~S` / `~D` /
`~F` / `~E` / `~%` などのディレクティブを実装する。

**主要なファイル**: `runtime/Runtime.Printer.cs` (2271 行)、
`runtime/Runtime.Format.cs` (3262 行)。

**実装上の論点**: シンボルの大文字小文字変換は readtable-case
(`Upcase` / `Downcase` / `Invert` / `Preserve`) と `*print-case*`
(`UPCASE` / `DOWNCASE` / `CAPITALIZE`) を組み合わせて決める。
`prin1` はシンボル名がエスケープを必要とするか (`SymbolNeedsEscaping`)
を判定して `|...|` で包む。Float の出力は `Numbers.cs` の `ToString`
側に集約されており、Printer は escape 文字列の組み立てに専念する。
循環検出 (`*print-circle*`) は事前走査でラベル付けする visitor で
実装。

**ANSI / SBCL との差分**: pprint logical-block 系 (`pprint-newline`
等) は最低限のみ。production-quality な pretty printer は
将来課題。`format` の `~/.../` 関数呼び出しディレクティブは対応済。

### 3.5 文字と文字列 (UTF-16 内部)

**データ表現**: `LispChar` は .NET の `char` (UTF-16 code unit、
16 bit) をラップ。0–127 はキャッシュ済み。`LispString` は copy-on-write
で初期は immutable な `string`、書き込み時に `char[]` を materialize
する。base-string と string の区別はなし (実装上は同じ)。

**主要なファイル**: `runtime/LispString.cs`、`LispChar` 関連は
`runtime/Numbers.cs` 内の char キャッシュ部、述語類は
`runtime/Runtime.Predicates.cs`。

**実装上の論点**: `format` / `prin1` の出力など read-only パスでは
copy-on-write のおかげで `char[]` のコピーが発生しない。書き込みが
入る `(setf (char ...))` や `nstring-upcase` で初めて materialize
される。`#\Space` / `#\Newline` 等の名前付き文字は `Runtime.CharName`
のテーブルで解決。

**ANSI / SBCL との差分**: .NET の `char` が UTF-16 code unit である
ため、補助面文字 (U+10000 以上、絵文字や CJK 拡張 B) はサロゲート
ペア (2 char) で表現され、CL の 1 文字として扱えない。`code-char` /
`char-code` は基本多言語面 (BMP) 範囲でのみ厳密に動く。完全 Unicode
対応は `System.Text.Rune` で `LispChar` の内部を `int` (code point)
に切り替える形で将来対応する想定 (優先度低、ASDF / conditions /
CLOS は ASCII 圏で完結するため当面問題なし)。

### 3.6 Lisp 製コンパイラ

**データ表現**: 入力は S 式 (Lisp フォーム)、出力は CIL 命令リスト
(`((:ldc-i8 1) (:call "Fixnum.Make") (:ret))` のような
キーワードシンボルのタプル列)。コンパイラは純粋関数で、副作用なし。

**主要なファイル**:
- `compiler/cil-compile.lisp` (145 行) — クロスコンパイル時のドライバ。
  ファイル読み込み、`eval-when` の `:compile-toplevel` 処理、SIL 出力
- `compiler/cil-compiler.lisp` (1286 行) — `compile-toplevel` /
  `compile-toplevel-eval` のエントリポイント、`*locals*` /
  `*specials*` / `*boxed-vars*` などのコンテキスト変数管理
- `compiler/cil-forms.lisp` (4698 行) — 250 ほどの special form ハンドラ
  を `*compile-form-handlers*` ハッシュ (O(1) ディスパッチ) に登録。
  `quote` / `if` / `let` / `lambda` / `block` / `tagbody` /
  `handler-case` / `unwind-protect` 等
- `compiler/cil-analysis.lisp` (665 行) — 自由変数解析
  (`find-free-vars-expr`)、変異解析 (どの変数を closure cell に
  ボックス化するか)
- `compiler/cil-stdlib.lisp` (1235 行) — `cons` / `car` / `mapcar`
  などの標準関数を Lisp で実装。C# 側の `Runtime.cs` と対になっており、
  `#'eql` のように関数オブジェクトで取りたい場合に Lisp 実装が必要

**実装上の論点**: 自由変数解析はワークリストで反復し、深くネストした
フォームでの再帰 stack 溢れを避ける。変異解析が「let で束縛されて
あとで `setq` される変数」を検出すると、その変数は `LispObject[1]`
の cell にボックス化されて closure 経由で共有される (D004)。
末尾呼出しは `(:tail-prefix)` ヒントを付けるが、try/catch / finally
の中では IL の制約 (try ブロック侵入時にスタックが空) によりヒントを
落とす。`eval-when` の compile-time 副作用 (defmacro など) は
cross-compile / load 双方で正しく走るように `cil-compile.lisp` 側で
明示的に処理 (D005, D010)。

**ANSI / SBCL との差分**: 命令リストは VM bytecode ではなく **CIL を
そのまま表現したデータ**。実行は CLR JIT に委ねる。SBCL のように
独自の VOP / IR1 / IR2 段を持たず、最適化は基本的に「素直な CIL を
出して JIT に任せる」スタンス。型推論ベースの最適化 (Step 7) は
未実装。

### 3.7 CIL アセンブラ

**データ表現**: 命令リスト (S 式) を受けて `ILGenerator` を叩く C#
の薄い層。74 種ほどの opcode / directive をサポート (内訳: 真の
CIL opcode が ~56、dotcl 特殊 directive が ~11、サブトークンが ~7)。
ECMA-335 の全 219 opcode に対するカバレッジは ~25% で、bit 演算 /
instance field LDFLD / prefix (`volatile.` / `constrained.`) などは
必要が出たら追加する方針。

**主要なファイル**: `runtime/Emitter/CilAssembler.cs` (2695 行、
ディスパッチ大スイッチ・定数プール・ラベル管理・例外フレーム検証)、
`runtime/Emitter/CompilerEnv.cs` (`VarKind` enum, lexical scope
chain, block / tagbody info)、`runtime/Emitter/IlDisasm.cs`
(逆アセンブラ、デバッグ補助)。

**実装上の論点**: ラベルは前方参照を許すので、分岐命令の処理時点で
未定義なら遅延解決する。dotcl 固有 directive は `:defmethod`
(関数定義の登録)、`:ldsym` (シンボル名 → `Startup.Sym` で
ロード時解決)、`:make-closure` (クロージャ生成) など。try ブロックは
CIL の制約 (進入時スタックが空) を assembler 側で検証し、違反する
emit を弾く (D683)。FASL モードでは定数文字列を assembly レベルで
intern してメソッド間で共有し、IL サイズを抑える。

**ANSI / SBCL との差分**: 同列に並ぶものはない (.NET 上の Lisp で
あって SBCL の VOP に相当する層が違う)。`ilverify` で静的検証可能な
レベルの CIL を出すように常に保つ — 不正な CIL を出すと
`InvalidProgramException` で実行時エラーになるため、生成側のバグを
早期に発見する道具として効く。

### 3.8 eval / load / compile-file (.sil / .fasl)

**データ表現**: 同じ命令リストが 2 つの経路で消費される。
- **動的経路 (eval / load)**: `DynamicMethod` + `ILGenerator` →
  即実行。constants pool で reference を保持し、寿命は GC が管理
- **静的経路 (compile-file)**: `PersistedAssemblyBuilder` →
  `.fasl` (PE .NET assembly) として書き出し。constants は IL に
  inline、ロード時に CLR が JIT する

中間フォーマットとして `.sil` (S 式テキストの IL) があり、ディスク上
で人間が読める形で命令リストを保存できる。`compiler/cil-out.sil` が
クロスコンパイル成果物。

**主要なファイル**: `runtime/Emitter/FaslAssembler.cs` (492 行、
`PersistedAssemblyBuilder` を駆動して `.fasl` を生成)、
`runtime/Emitter/DynamicClassBuilder.cs` (645 行、
`dotnet:define-class` 経由で実 .NET クラスを emit、3.16 と関連)、
`compiler/cil-compile.lisp` (compile-file ドライバ)。

**実装上の論点**: 動的経路と静的経路で constants の扱いが異なる。
動的では `_constants` 配列にぶら下げ、静的ではすべて IL に展開
(persisted assembly は外部参照を持てないため)。`load-time-value` は
sequential ID (`*ltv-counter*`) を振り、`Startup.LoadTimeValueSlot`
経由で遅延評価する。`.fasl` は **IL only / OS・CPU 非依存** で、
Windows でビルドした `.fasl` を Linux で load しても動く (NativeAOT
を使わない限り)。配布の `dotcl.core` も `.fasl` (D675、前身 D673 が
`.sil`)。`module-provide-contrib` は `.fasl` → `.sil` → `.lisp` の順
で contrib を解決する。

**ANSI / SBCL との差分**: SBCL の FASL は machine-code を含むため
プラットフォーム固有だが、dotcl の `.fasl` は IL のみで cross-platform。
代わりに SBCL のような起動時のネイティブコード即実行はできず、
load 時に CLR JIT が走る。`.fasl` 0.73s / `.sil` 1.77s / `.lisp` 3.38s
(D675 計測、ASDF.fasl ロード時間) と `.fasl` が圧倒的に速い。

### 3.9 動的束縛 (special variables)

**データ表現**: `ThreadStatic` の平坦スタック (`Symbol[]` と
`LispObject[]` のペア、容量は倍増)。`null` = unbound。スタック頂点
からの逆順走査で最新の binding を O(1) (ヒット時) 〜 O(d) (深い検索)
で取得する。

**主要なファイル**: `runtime/DynamicBindings.cs` (208 行、Snapshot /
Restore も含む完全実装)。

**実装上の論点**: シンボルが special かどうかは `*global-specials*` /
`*specials*` の registry で持つ。`progv` は任意のシンボルを動的に
バインド可能。スレッド生成時は親の binding stack を `Snapshot()`
して子に `Restore()` し、SBCL の per-thread binding 継承と同等の
挙動を実現する。binding 数が膨らむと逆順走査が遅くなる可能性は
あるが、現状は実用範囲。

**ANSI / SBCL との差分**: ANSI 準拠。SBCL の per-symbol thread-local
slot index 化のような高速化は未実装 (binding 数が SBCL ほど多くない
想定で、stack 走査で十分という判断)。

### 3.10 多値

**データ表現**: 通常の戻り値は 1 つの `LispObject`。多値が必要な経路
では `MvReturn` ラッパー (`LispObject[] Values`) を返す。
ThreadStatic のサイドチャネル (`_count` / `_values`) で「最後の明示的
`(values ...)` 呼び出し」をキャッシュし、`multiple-value-bind` 等が
ラッパーなしで読めるようにする。

**主要なファイル**: `runtime/MultipleValues.cs` (80 行)。

**実装上の論点**: 「MV reset」 (1 値しか期待していない位置で多値が
漏れない保証) は `_count = -1` の sentinel で表現し、`_values`
配列自体は触らない (ThreadStatic 書き込み回数の削減)。`unwind-protect`
の cleanup 中に多値が破壊されないよう `SaveCount` / `SaveValues`
/ `RestoreSaved` で退避する。多値のヒープアロケーションは SBCL の
レジスタ渡しに比べると遅いが、頻度が低いので問題化していない。

**ANSI / SBCL との差分**: ANSI 準拠。dotcl は `(values)` (0 値) と
2 値以上のときだけラッパーを生成し、単値は素通し。

### 3.11 コンディションとリスタート

**データ表現**: `LispCondition` (型名・format-control・arguments)
を `LispErrorException : Exception` でラップして .NET 例外機構に
乗せる。`HandlerClusterStack` / `RestartClusterStack` (どちらも
ThreadStatic な `List<HandlerBinding[]>` / `List<LispRestart[]>`)
で handler-bind / restart-bind の階層を持つ。

**主要なファイル**: `runtime/Conditions.cs` (549 行、stack 構造と
基本クラス)、`runtime/Runtime.Conditions.cs` (943 行、`signal` /
`error` / `warn` / `restart-case` / `invoke-restart` API)。

**実装上の論点**: `handler-case` は CIL の try/catch +
`HandlerCaseInvocationException` で非局所脱出 (巻き戻し型)。
`handler-bind` は `Signal()` がスタックを下りながら検索し、マッチ
したクラスタを除去してハンドラを呼ぶ (再帰 signal の防止)。
`handler-bind` のハンドラが return すれば signal は伝播継続。
`error` / `warn` は `ConditionSystem.Error` / `Warn` の単一エントリ
ポイントから入る (D007)。`handler-case` は raw .NET 例外も catch
する (D034) — `(handler-case ... (error () ...))` で
`NullReferenceException` 等が捕まる。Restarts は restart-bind で
スタックに積み、`_conditionRestarts` で対象 condition と関連付ける。

**ANSI / SBCL との差分**: ANSI 準拠。condition 型は CLOS class
として定義 (`define-condition` は `defclass` の制限版)。SBCL の
ような stack frame キャプチャによる restart 表現ではなく、明示的な
struct stack を持つ。

### 3.12 CLOS / MOP

**データ表現**: `LispClass` (Symbol Name, DirectSlots, CPL array,
EffectiveSlots array, SlotIndex dict)、`LispInstance` (Class,
Slots — null = unbound)、`LispMethod` (Specializers, Qualifiers,
Function body)、`GenericFunction : LispFunction` (Methods list,
single-entry `LastDispatch` cache, MethodCombination)、`SlotDefinition`
(name, initargs, initform thunk, IsClassAllocation)。

**主要なファイル**: `runtime/Clos.cs` (404 行、class / instance /
method の C# 実装)、`runtime/Mop.cs` (322 行、closer-mop 互換シンボル
と introspection API: `class-slots` / `generic-function-methods` 等)、
`runtime/Runtime.CLOS.cs` (3566 行、`make-instance` / dispatch /
`defclass` 展開)。

**実装上の論点**: CPL は C3 linearization。slot マージは initargs
union、initform / allocation は最特異性優先。`make-instance` は
default-initargs / shared-initialize / class slot のレイアウトに
よってキャッシュが効くと判断したら `CanUseFastMakeInstance` で
高速パスを通る (D226)。Method dispatch は monomorphic inline cache
(1 entry) で primary / before / after / around を group し、
`CachedDispatch` でクラスタプル一致時にキャッシュ命中。EQL specializer
は `(eql X)` の cons で表現。クラス再定義は in-place 更新 + dependents
の re-finalize (D165)。

**ANSI / SBCL との差分**: ANSI 準拠。SBCL のような metaclass 階層を
細かく実装する代わりに `IsBuiltIn` / `IsStructureClass` 等の flag
で簡略化。dispatch cache は 1 エントリの IC のみ (SBCL は polymorphic
inline cache + DAG)。Method combination は string registry ベースで
基本的なものだけ提供。

### 3.13 マクロ / setf / LOOP

**データ表現**: マクロ展開器は `*macros*` ハッシュテーブル
(`macro-name → expander-lambda`)、setf expander は
`*setf-expanders*` (`accessor-name → expander-lambda`) と
`*setf-expansion-fns*` (define-setf-expander 用、5 値返し)。LOOP は
`compiler/loop.lisp` (2530 行) に MIT LOOP (Symbolics / Glenn
Burke 系) の移植を保持。

**主要なファイル**: `compiler/cil-macros.lisp` (3893 行、defmacro /
setf 系の expander 一式)、`compiler/loop.lisp`。

**実装上の論点**: マクロ登録は `eval-when (:compile-toplevel ...)`
の compile-time 副作用として `(setf (gethash 'name *macros*)
expander)` で行う。setf のキーは CL シンボルなら bare name (`"CAR"`)、
それ以外は `"PKG:NAME"` で qualified (D183)、ルックアップは qualified
→ bare の fallback。`destructuring-bind` は `&rest` / `&optional` /
`&key` / `&aux` を `%db-bindings` で展開し、ネスト分解にも対応する。
LOOP は ANSI 標準の機能のみ (Genera 拡張は載せない)。

**ANSI / SBCL との差分**: ANSI 準拠。LOOP の出自から SBCL の LOOP
と同等の振る舞いをする。マクロ展開は read 時 (バッククォート展開)
+ compile 時 (`*macros*` 検索) の 2 段で、再帰展開上限などの
implementation limit は緩く取っている。

### 3.14 スレッド

**データ表現**: `LispThread` は `.NET Thread` のラッパ、`LispLock` は
`Monitor`、`LispConditionVariable` は `Monitor.Wait` / `Pulse`、
`LispSemaphore` は `SemaphoreSlim` をラップする。bordeaux-threads
互換の API を提供する。

**主要なファイル**: `runtime/Runtime.Thread.cs` (414 行、
`bt:make-thread` / `acquire-lock` / `condition-wait` 等の組み込み)。

**実装上の論点**: 親スレッドの動的束縛を `Snapshot()` して子で
`Restore()` (3.9 と連動)。`.NET Monitor` は再入可能なので、
`make-lock` と `make-recursive-lock` の差はフラグだけの semantic 区分。
`destroy-thread` は .NET 5+ で `Thread.Abort` が削除されているため
`Thread.Interrupt()` で代替する softer な実装にしている (#176)。
グローバル状態のスレッドセーフ化は #171 の連続作業で、3.2 (Symbol
の volatile 化) も同じ流れ。

**ANSI / SBCL との差分**: CL は ANSI でスレッドを規定していないので、
互換性の基準は bordeaux-threads。SBCL `sb-thread:thread` と異なり、
スケジューリングは .NET の ThreadPool / OS スケジューラに完全に委ねる。

### 3.15 ASDF / module loader

**データ表現**: `(require "name")` で `module-provide-contrib`
が探索パスを順に走り、`<name>.fasl` → `<name>.sil` → `<name>.lisp`
の順で解決する。配布物の ASDF は `runtime/contrib/asdf/asdf.fasl` を
同梱しているので、ユーザは `(require "asdf")` するだけで使える。

**主要なファイル**: `runtime/Runtime.Misc.cs` 内の
`ModuleProvideContrib`、配布バンドル `runtime/contrib/asdf/asdf.fasl`、
ASDF 本体は別 fork (`github.com/dotcl/asdf`) の master ブランチを
`make setup-asdf` で取り込む。

**実装上の論点**: `.fasl` は cross-platform .NET IL なので Windows /
Linux / macOS の x86-64 / ARM64 で同じバイナリが動く。`.fasl` 0.73s
/ `.sil` 1.77s / `.lisp` 3.38s の load 時間差により ASDF のような
大物は `.fasl` 強制 (D675)。同名モジュールの 2 重 load は
`_modulesLock` で防ぐ。`asdf/` 以下のソースを修正したら
`make setup-asdf` → `make compile-asdf-fasl` → `make pack` で
`.fasl` が再生成される。

**ANSI / SBCL との差分**: ASDF / Quicklisp 生態系のかなりの部分が
SBCL 内部に依存しないなら動く (alexandria / bordeaux-threads 等は
そのまま load できる)。SBCL 専用のアセンブラや sb-vm を直接叩く
実装は当然動かない。

### 3.16 .NET 相互運用 (dotnet: パッケージ)

**データ表現**: `LispDotNetObject(Value, Type)` が任意の .NET
オブジェクトをラップし、`#<DOTNET FullName value>` で印字される。
`LispDotNetBoxed(Value, HintType)` は overload 解決のための型ヒント。
`dotnet:define-class` で emit されるクラスは **本物の CLR クラス**
(public、Reflection から見える、interface / 継承可能)。

**主要なファイル**: `runtime/Runtime.DotNet.cs` (834 行、`dotnet:new`
/ `dotnet:invoke` / `dotnet:static` / Lisp ↔ .NET marshalling)、
`runtime/Emitter/DynamicClassBuilder.cs` (645 行、`DefineMinimalClass`
/ `EmitLispDispatchMethod` / `EmitAutoProperty` / interface 自動実装)、
`runtime/Runtime.NuGet.cs` (`dotnet:require` で nuget.org から DL し
`Assembly.LoadFrom`)。

**実装上の論点**: `dotnet:define-class` は呼び出しごとに
`AssemblyBuilder` (`DotclDynamic_<n>`) を新規生成し、その中に
`TypeBuilder` を 1 つ作る。メソッド本体は public instance method
として emit され、`DispatchLispMethod` (グローバル辞書、key は
`(typeFullName, methodName)`) を経由して Lisp ラムダにディスパッチ
する。auto property / event / 属性 (Attribute) も CLR メタデータと
して正しく emit するので、MAUI の binding や ASP.NET Core の routing、
JSON シリアライザの自動 discover がそのまま効く。NuGet 統合は
`~/.nuget/packages/` にダウンロードしてフレームワーク整合性のある
DLL を `Assembly.LoadFrom` で取り込む方式。

**ANSI / SBCL との差分**: ANSI 範囲外の dotcl 拡張。SBCL の CFFI が
foreign function call に閉じているのに対し、dotcl の `dotnet:` は
**CLR の同一型システム上で Lisp のクラスが定義される** ため、MAUI /
ASP.NET Core / MonoGame といったフレームワークがそのクラスを「ただの
.NET の型」として扱える (`samples/` の MauiLispDemo / AspNetLispDemo /
MonoGameLispDemo / McpServerDemo 参照)。

## 4. ディレクトリ構成

```
dotcl/
  compiler/    Lisp 製 CIL コンパイラ (cil-compiler.lisp / cil-forms.lisp /
               cil-stdlib.lisp / cil-analysis.lisp / cil-macros.lisp /
               cil-compile.lisp / loop.lisp)。cross-compile が
               cil-out.sil (S 式 IL) を生成し、ランタイム起動時に
               読み込む
  runtime/     C# ランタイム (.NET 10)。LispObject 階層、Reader、CIL
               assembler (Emitter/CilAssembler.cs と FaslAssembler.cs)、
               組み込み関数。機能別に Runtime.*.cs と LispObject 由来
               クラスに分割
  contrib/     Lisp 製拡張置き場 (ASDF を中心に、雑多なモジュールを
               含む — 公開向けの整理は未着手)
  samples/     dotcl を host する .NET 統合サンプル (MauiLispDemo /
               AspNetLispDemo / MonoGameLispDemo / McpServerDemo)
  examples/    Lisp スニペット集 (Windows interop など)
  docs/        decisions/ (D 番号付き個別判断記録 ~880 件)、
               plans/ (実装前設計メモ)、windows.md
  test/        regression/ (dotcl 固有回帰テスト)、framework.lisp
  bench/       benchmark スイート
  Makefile     build オーケストレーション
  CHANGELOG.md 時系列作業ログ
  README.md    ユーザ向け入口
```

## 5. ロードマップ (履歴)

ここに至るまでの段階。現在は ASDF が動き、ansi-test が 99.99% 通る
状態 (Step 6 まで達成)。

- **Step 1**: ランタイムカーネル (LispObject 階層、数値塔、パッケージ、
  Reader、コンディション/restart 基盤、動的束縛、多値、REPL)
- **Step 2**: C# テキスト生成コンパイラ (プロトタイプ、CIL 前の概念
  実証)
- **Step 3**: CIL エミッタの C# 概念実証 (`Reflection.Emit` 直叩き)
- **Step 4**: Lisp 製 CIL コンパイラ (A2 方式)。eval / load /
  compile-file が同一コンパイラを通り、`DOTCL_LISP=dotcl make
  cross-compile` でセルフビルド可能
- **Step 5**: CL 機能の拡充 (defmacro / loop / 多値 / 型 / defstruct
  / コンディション / CLOS / pathname / compile-file)
- **Step 6**: ASDF ロードと ansi-test 21,928 / 21,929 (99.99%) 達成
- **Step 7 (未着手)**: 最適化パス (型推論、unboxed 数値演算、
  インライン展開) — 命令リストに対するパスとして挟む A2 の利点を
  活かす。SBCL の IR1 を参考にしつつ CIL 向けの軽量 IR を別途設計
- **Step 8 (部分達成)**: セルフホスト。Step 6.2 で Lisp 製コンパイラ
  が dotcl 上で自身をコンパイル可能に。残りは save-lisp-and-die 相当
- **Step 9 (未着手)**: Swank / Lem 接続。TCP ソケット +
  bordeaux-threads + `swank-dotcl.lisp` (defimplementation)。
  Phase 1 (TCP) と Phase 2 (スレッド基盤) は並行可能

個別の修正記録は `CHANGELOG.md` と `docs/decisions/`、未解決課題は
GitHub Issues。

## 6. 技術的参考

- **ABCL** (JVM 上の Common Lisp): 独自コンパイラ + Java ランタイム。
  Lisp on managed runtime の先行例。
- **IronScheme** (.NET 上の Scheme): C# でランタイム、DLR 活用。
- **System.Reflection.Emit**: `DynamicMethod` (軽量、GC 回収可能) と
  `PersistedAssemblyBuilder` (.dll 出力可能、.NET 9+ で復活) の 2 モード。
- **MIT LOOP** (Symbolics / Glenn Burke 系): `compiler/loop.lisp` の
  出自。

## 7. 設計判断記録

個別の設計判断は `docs/decisions/D{NNN}-...md` に背景・選択肢・根拠と
ともに記録 (~880 件)。`CHANGELOG.md` は時系列の作業ログ、本ファイルは
機構別実装ノートに集中する。
