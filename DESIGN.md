# dotcl - Common Lisp on .NET

.NET 上で動く Common Lisp 処理系。Lisp ソースを CIL (Common
Intermediate Language) にコンパイルし .NET の JIT で実行する。
ANSI 規格適合は ansi-test (gitlab.common-lisp.net/ansi-test/ansi-test)
で 21,944/21,945 (99.99%) pass。CIL を出さない解釈経路 (§3.17) も
持ち、Reflection.Emit が使えない環境 (netstandard2.0 / AOT) では
そちらで動く。

このドキュメントは dotcl の **現状の実装** を機構ごとにまとめる
ノートで、「いま何がどう動いているのか」を実装機構別に記述する。

CLHS や SBCL と**意図的にずらしてある挙動**は 1 枚にまとめてある:
`docs/deviations.md`。

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
fixnum 表現は既定では持たず、すべての数値はヒープオブジェクト。ただし
型宣言に基づく unboxing は要所で導入済み: `(array single-float)` /
`(array double-float)` は要素を raw な `float[]` / `double[]` で保持し
(box なしの `aref` / `setf aref`)、型推論で float 配列と判定した local は
native な `r8` slot で持ち回るので `setq` 毎の box が消える (daxpy 等の
数値カーネルで大幅な box 削減)。スカラ fixnum の全面 unboxing は引き続き
将来課題。

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
フィールドの選択はグローバル状態のスレッドセーフ化の第 1 段で、
個別の per-symbol ロックは将来必要になったら入れる。

**ANSI / SBCL との差分**: `setf` 関数を専用レジストリではなくシンボル
直属の `SetfFunction` フィールドに置く ((setf foo) を高速に解決する
ための選択、Phase 1)。それ以外は ANSI 通り。

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
バッククォート展開は **read 時** に行われ、結果には quasiquote
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
- `compiler/cil-compile.lisp` (184 行) — クロスコンパイル時のドライバ。
  ファイル読み込み、`eval-when` の `:compile-toplevel` 処理、SIL 出力
- `compiler/cil-compiler.lisp` (3450 行) — `compile-toplevel` /
  `compile-toplevel-eval` のエントリポイント、コンパイル状態
  (`*CSTATE*` パック、下記) と大域環境 (`*macros*` / `*specials*` 等)
  の管理、inline 展開・compiler macro のフック
- `compiler/cil-forms.lisp` (7388 行) — 250 ほどの special form ハンドラ
  を `*compile-form-handlers*` ハッシュ (O(1) ディスパッチ) に登録。
  `quote` / `if` / `let` / `lambda` / `block` / `tagbody` /
  `handler-case` / `unwind-protect` 等
- `compiler/cil-analysis.lisp` (1216 行) — 自由変数解析
  (`find-free-vars-expr`)、変異解析 (どの変数を closure cell に
  ボックス化するか)
- `compiler/cil-stdlib.lisp` (1705 行) — `cons` / `car` / `mapcar`
  などの標準関数を Lisp で実装。C# 側の `Runtime.cs` と対になっており、
  `#'eql` のように関数オブジェクトで取りたい場合に Lisp 実装が必要

**コンパイル状態の持ち方**: per-compilation の文脈 (スコープ表
`*locals*` 相当・native 表現スロット表・TCO 文脈など 23 種) は単一の
special `*CSTATE*` が持つ simple-vector のスロットに集約されている。
更新は常に functional (コピーして `*CSTATE*` を let 再束縛 / setq)。
リセット規則はこれで 1 箇所に落ちる: クロージャ境界は空パックへの
束縛 (レジストリ経由・実 4 エントリ)、非クロージャ関数本体は
`cstate-fresh-function-body` (呼び出し元が渡す self-TCO の受け渡し
2 スロットだけ保存)。スロットを足せば全境界のリセットに自動参加する
ので、「リセット列挙への追加漏れ → 内側 body が外側文脈を引きずる
silent miscompile」というバグクラスが構造的に消える。式ごとに高頻度で
再束縛される 2 フラグ (`*in-tail-position*` / `*in-mv-context*`) だけは
dynamic binding のまま (プロファイル上 special アクセスは無視できる
コストで、頻繁な再束縛には dynamic binding が正しい道具)。変数の
参照・シャドウ判定はシンボル identity で行い、native 表現スロット表は
変数名でなくスロット key でキーする — 同名別 package や内側の再束縛が
別スロットに解決されることで、シャドウ処理そのものが不要になる。

**inline 展開**: `(declaim (inline f))` された全必須引数の関数は、
呼び出し地点で `(let ((p a)...) decls (block f body))` に展開して
その場でコンパイルする (即時適用 lambda は β簡約されないので使わない)。
局所関数によるシャドウ・NOTINLINE・再帰・サイズ超過・名前捕獲の
恐れがあるときは普通の呼び出しに落とすだけなので、拒否側に間違いは
ない。展開体は呼び出し元の tail/TCO 文脈を継承する — これは意図した
動作で、tail 位置で inline された body 内から包含関数への tail call は
包含関数の TCO ループに乗る (深い相互再帰が inline 越しに回る)。

**実装上の論点**: 自由変数解析はワークリストで反復し、深くネストした
フォームでの再帰 stack 溢れを避ける。変異解析が「let で束縛されて
あとで `setq` される変数」を検出すると、その変数は `LispObject[1]`
の cell にボックス化されて closure 経由で共有される。
末尾呼出しは `(:tail-prefix)` ヒントを付けるが、try/catch / finally
の中では IL の制約 (try ブロック侵入時にスタックが空) によりヒントを
落とす。`eval-when` の compile-time 副作用 (defmacro など) は
cross-compile / load 双方で正しく走るように `cil-compile.lisp` 側で
明示的に処理する。

**ANSI / SBCL との差分**: 命令リストは VM bytecode ではなく **CIL を
そのまま表現したデータ**。実行は CLR JIT に委ねる。SBCL のように
独自の VOP / IR1 / IR2 段を持たず、最適化は基本的に「素直な CIL を
出して JIT に任せる」スタンス。ただし局所的な型推論ベースの最適化は
導入が始まっており、float 数値配列 / 要素型宣言から unboxed な
`float[]` / `double[]` ストレージと native float local を選ぶ経路が
その第一歩 (3.1 参照)。呼出側では、名前付き関数や共通アリティの
組込みに per-arity の direct delegate を装着して引数配列パック
(InvokeSlow) を回避する最適化を進めている。クロージャも同じ扱いで、
本体を `(env, a0..aN-1)` 署名で emit すれば直行できる。**これは
`.fasl` にも適用される** — 適用していなかった間は、同じソースでも
`compile-file` した途端に呼び出しごとに引数配列を確保していた
(quickload したライブラリは全部 `.fasl` なので、そちらが通常の形)。
全面的な型推論最適化は引き続き将来課題。

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
emit を弾く。FASL モードでは定数文字列を assembly レベルで
intern してメソッド間で共有し、IL サイズを抑える。

**ANSI / SBCL との差分**: 同列に並ぶものはない (.NET 上の Lisp で
あって SBCL の VOP に相当する層が違う)。`ilverify` で静的検証可能な
レベルの CIL を出すように常に保つ — 不正な CIL を出すと
`InvalidProgramException` で実行時エラーになるため、生成側のバグを
早期に発見する道具として効く。

**検証ゲート**: `make ilverify` が (1) 生成したてのコア (cil-stdlib +
コンパイラ全体)、(2) `test/ilverify/stress.lisp` (emit を多く踏む
フィクスチャ)、(3) `contrib/asdf/asdf.fasl` を検証する。3 番目が効くのは
それが「この検査のために書かれていない実コード」だから。CoreCLR の JIT は
共変呼び出しや i4/i8 のずれを黙認するが、IL2CPP / WebGL のような AOT
codegen は拒否するので、25 分の AOT ビルドまで発覚を遅らせないための門。
CI で毎 push 走る。

### 3.8 eval / load / compile-file (.sil / .fasl)

**データ表現**: 同じ命令リストが 2 つの経路で消費される。
- **動的経路 (eval / load)**: `DynamicMethod` + `ILGenerator` →
  即実行。constants pool で reference を保持し、寿命は GC が管理
- **静的経路 (compile-file)**: `PersistedAssemblyBuilder` →
  `.fasl` (PE .NET assembly) として書き出し。constants は IL に
  inline、ロード時に CLR が JIT する

中間フォーマットとして `.sil` (S 式テキストの IL) があり、ディスク上
で人間が読める形で命令リストを保存できる。`compiler/cil-out.sil` が
クロスコンパイル成果物。コアの命令列は 1 本の巨大メソッドではなく、
ファイルを跨がない **32 トップレベルフォーム単位のセグメント**を
`(:toplevel-boundary)` で連結した形で emit する。ローダは境界で分割して
1 セグメント = 1 メソッドとして実行するので、組み立て・JIT のピークが
セグメント単位になり、core ロード時の peak working set が約 4 割下がった
(セグメント数に対しピークは U 字で、per-form まで細かくすると逆に
固定費で太る。粒度は `DOTCL_SEG_FORMS` で上書き可)。

**主要なファイル**: `runtime/Emitter/FaslAssembler.cs` (492 行、
`PersistedAssemblyBuilder` を駆動して `.fasl` を生成)、
`runtime/Emitter/DynamicClassBuilder.cs` (645 行、
`dotnet:define-class` 経由で実 .NET クラスを emit、3.16 と関連)、
`compiler/cil-compile.lisp` (compile-file ドライバ)。

**実装上の論点**: 動的経路と静的経路で constants の扱いが異なる。
動的では `_constants` 配列にぶら下げ、静的では **持ち出せる形に
落とし直す** (persisted assembly は外部プロセスのオブジェクトを
参照できないため)。落とし方は 2 通りあり、大きさで選ぶ:

- **小さいリテラル**: IL で 1 要素ずつ組み立てる
- **大きいリテラル (閾値 8 ノード)、および循環・共有のあるグラフ**:
  印字表現を **UTF-8 で PE のデータセクション** (`DefineInitializedData`)
  に置き、ロード時に reader で復元する。組み立て IL は 1 回実行する
  ためだけに必ず JIT されるので、リテラルが支配的なファイルでは
  **ロード時間の大半が JIT** になっていた (実測: リテラルが IL の 93% を
  占める合成ファイルで load 壁時計の 97%)。データにすると load が
  10-20 倍速く、コードのために積むメモリが 8 分の 1 になる。
  意味論が変わらないグラフに限る — パス名 (version が落ちる)、
  単純ベクタ以外のベクタ (要素型が落ちる)、CLOS インスタンスは
  組み立て経路に残す。表現は `*print-circle*` で印字し、`*package*` /
  `*readtable*` / `*read-default-float-format*` を固定して読む
  (どれか 1 つでも読む側の設定に委ねると、値は合っているのに型や
  シンボルが変わる)

判定はリテラル 1 個をまるごと 1 単位で行うので、**読めないノードが
1 つ混ざると全体が組み立て経路に落ちる**。ここを広げる方向で 3 段階
動いた:

- **未 intern シンボル**: fasl ごとの表への添字として `#<n>U` で印字し、
  reader は表の実体を返す。EQ がリテラル内でもリテラル間でも保つので、
  除外条件から外した
- **構造体**: `#K` で印字する。`make-load-form` がまさに
  「割り付けてスロットを埋める」だけを求めている場合に限る (判定は
  クラス単位でなく**インスタンス単位** — 分岐する make-load-form が
  実在する)。`make-load-form-saving-slots` の生成フォームを s 式
  リテラルとして組み立てて load 時に `Eval` していた経路が消える。
  生成フォームが slot-saving の形と一致するなら emitter 側でも
  `Eval` を通さず直接 intern する
- **閾値**: 64 は合成ファイルで測った値で、そこには構造体が無かった。
  実ライブラリでは閾値未満のリテラルに入った構造体が `Eval` 経路に
  残り続けていたので 8 に下げた

**リテラルは静的フィールドに一度だけ**組み立てる。本体に直接置くと
呼び出しのたびに作り直され、`(eq (f) (f))` が NIL になり破壊的変更も
消える (CLHS 3.2.4.4 違反)。**ソースから load する開発木では再現せず、
`.fasl` で配った形でだけ壊れる**ため長く見えていなかった。
同じ性質の無言の破損を機械で捕まえる道具が 2 つある:
`DOTCL_LITERAL_VERIFY=1` (印字 → 読み直し → 印字の往復をその場で検算)、
`DOTCL_LITERAL_CENSUS` (拒否されたリテラルの内訳)。

**fasl の「形」の CI ガード**: 壊れ方が load 側にしか出ず、総バイト数にも
コンパイル時間にも現れない種類の退行があるので、`make test-fasl-shape`
が最大メソッド IL バイト数・型あたりフィールド/メソッド数・`#US` ヒープを
閾値付きで検査する。CLHS 3.2.3.1 の 5 つの包み (PROGN / EVAL-WHEN /
LOCALLY / MACROLET / SYMBOL-MACROLET) で flattener が降りるのをやめると
単一の巨大メソッドに潰れるので、そこを踏む fixture を置いてある

`load-time-value` は
sequential ID (`*ltv-counter*`) を振り、`Startup.LoadTimeValueSlot`
経由で遅延評価する。`.fasl` は **IL only / OS・CPU 非依存** で、
Windows でビルドした `.fasl` を Linux で load しても動く (NativeAOT
を使わない限り)。配布の `dotcl.core` も `.fasl`。
`module-provide-contrib` は `.fasl` → `.sil` → `.lisp` の順で
contrib を解決する。

**R2R 兄弟 fasl**: load 時の JIT は上の「IL のみ」の代償なので、そこを
ahead-of-time に倒す経路を別に持つ。`foo.fasl` の隣に
`foo.fasl.r2r-<rid>` (crossgen2 が同じモジュールをネイティブ化したもの)
があれば load はそちらを使い、無ければ元の `.fasl` に落ちる。設計上の
決めは 4 つ:

- **置き換えではなく兄弟**。`.fasl` は asdf がソースと新旧を比べる成果物の
  ままなので、計画した対象がすり替わっていることを誰にも教えなくてよい。
  兄弟が `.fasl` より古ければ無視するので、作り直した fasl が古い
  ネイティブコードに黙って上書きされることがない
- **パスで読む**。既定の fasl ロードは `Assembly.Load(byte[])` だが、
  バイト配列から読んだアセンブリの R2R コードは CLR が使わない。
  兄弟だけ `Assembly.LoadFrom` で読む
- **目印は拡張子の後ろ** (`*.fasl.r2r-<rid>`、ステム側ではない)。
  ステムに入れると `*.fasl` に一致してしまい、fasl を列挙する側
  (Makefile・2 つの csproj・crossgen2 のループ) が同じ除外を 4 回書く
  ことになっていた
- **可否は環境変数でなくスペシャル変数**。`dotcl:*compile-r2r*` (既定
  NIL、asdf が fasl をコンパイルしたら兄弟も作る) と `dotcl:*load-r2r*`
  (既定 T、読むとき兄弟を使う)。同じビルドでも「このシステムのコンパイル
  の間だけ」が書けるべきで、それは処理系の設定ではなく image の判断。
  環境変数は初期値を決めるだけに降格した

作る側は asdf の `perform :after ((compile-op) (cl-source-file))` から
`dotcl:write-r2r-sibling` を呼ぶ。`compile-file` の中ではフックできない
— asdf は一時名でコンパイルして後からリネームするので、最終パスを
知っているのは asdf の側だけ。この経路の失敗は全部「動くが遅い」なので
外から成功と区別がつかず、観測手段を 2 つ付けてある:
`(dotcl:r2r-stats)` が (兄弟から読んだ数 . 読んだ fasl の総数) を返し、
crossgen2 が無いなど**頼まれて出来なかったとき**はプロセスごとに 1 度だけ
復旧手順を stderr に書く (頼まれていないときは何も言わない)。

**fasl が運ぶ情報は SIL より狭くなりうる**。`.sil` を読む経路は
`:defmethod` ディレクティブの `:lambda-list` をそのまま `StoredLambdaList`
に入れるが、fasl の書き出しがそれを見ておらず、**core から起動した image
だけラムダリストを失っていた** (`.sil` 起動で 160/663、core 起動で 13/663。
差は `cil-stdlib.lisp` で Lisp で書かれた標準関数)。配布物は core 側なので、
インストールした dotcl では常にこちらだった。fasl でも運ぶようにしたが、
**リストとしてではなく書かれたままのテキストとして**運ぶ — fasl には定数
プールが無く、関数ごとにリストを組み立てる IL を起動時に走らせると、
ほとんど誰も読まない情報のために毎回の起動が払う。`StoredLambdaList` に
文字列のまま置き、`dotcl:function-lambda-list` が聞かれたときに 1 度だけ
読み替える (移植可能なラムダリストは変数名が未 intern シンボルなので、
読み戻しにパッケージが要らない)。

**ANSI / SBCL との差分**: SBCL の FASL は machine-code を含むため
プラットフォーム固有だが、dotcl の `.fasl` は IL のみで cross-platform。
代わりに SBCL のような起動時のネイティブコード即実行はできず、
load 時に CLR JIT が走る (上の R2R 兄弟はこの差を per-RID の追加成果物
として埋める形で、`.fasl` 自体は OS・CPU 非依存のまま)。実測 (ASDF を
`.fasl` / `.sil` / `.lisp` でロードした時間) で `.fasl` 0.73s /
`.sil` 1.77s / `.lisp` 3.38s と `.fasl` が圧倒的に速い。

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
**コストが出るのはラッパーではなく単値側の publish** — 1 値の戻りでも
「これが最後の値だ」をサイドチャネルに書き、その前に `is MvReturn` を
判定する。precompiled なコードを実行するだけのプロファイルでは、
型チェック時間の 45% がこの経路 (`MultipleValues.Primary` +
`UnwrapMv`) に出る。減らすには呼び出し規約の側を触ることになるので、
別課題として切ってある。

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
ポイントから入る。`handler-case` は raw .NET 例外も catch
する — `(handler-case ... (error () ...))` で
`NullReferenceException` 等が捕まる。Restarts は restart-bind で
スタックに積み、`_conditionRestarts` で対象 condition と関連付ける。

**深い再帰とスタック枯渇**: .NET の `StackOverflowException` は捕捉
不能でプロセスが即死するので、そこへ行かせない。`Runtime.Apply` や
interop の境界など深くなる経路で残スタックを事前に測り
(`RuntimeHelpers.EnsureSufficientExecutionStack` 系)、足りなければ
`STORAGE-CONDITION` として signal する。プローブは「投げるのに足る
64KB」では足りない — signal 自体がコンディションシステムを回すので、
16 フレーム × 16KB の余白を確保してから判定する。`storage-condition` は
仕様どおり `error` の**外**に置いてあるので `(handler-case ... (error ...))`
では捕まらない (SBCL と同じ)。netstandard2.0 には `Try` 形のプローブが
無いが、投げる形の同 API はあるのでそれを catch して使う。

**ANSI / SBCL との差分**: ANSI 準拠。condition 型は CLOS class
として定義 (`define-condition` は `defclass` の制限版)。SBCL の
ような stack frame キャプチャによる restart 表現ではなく、明示的な
struct stack を持つ。

#### Lisp と CLR の境界での振る舞い

condition と .NET 例外は同じ機構に乗っているので、境界を越えるときの
規則は「変換」ではなく「どちらの捕まえ方が効くか」で決まる。

| 経路 | 振る舞い |
| --- | --- |
| Lisp → .NET 呼び出しで .NET 例外 | `handler-case` が raw 例外をそのまま捕まえる。`dotnet:new` は `TargetInvocationException` の inner を剥がす。socket I/O のように意味が対応するものは `STREAM-ERROR` 等へ寄せてある |
| .NET → Lisp コールバックから condition が脱出 | `LispErrorException` として C# 側へ透過する。境界でハンドラクラスタを積むので、対話デバッガに落ちることはない。`storage-condition` だけは境界で封じ込める |
| REPL のトップレベル | `error` / `break` / `invoke-debugger` に加え、ランタイムが signal する condition (型違い・未定義関数・添字はみ出し・ゼロ割・.NET 例外の包み) も、誰も handle しなければデバッガに入る。入口は `LispErrorException` のコンストラクタ (ハンドラを走らせた直後、まだ何も巻き戻っていない時点) で、REPL が 1 フォームの評価と印字の間だけ立てるスレッドごとのスイッチ `ConditionSystem.UnhandledErrorsEnterDebugger` で有効になる。スクリプト・ライブラリの C# 側 `catch`・REPL の reader (未完のフォームは END-OF-FILE で続きを待つ) には効かない。デバッガ/`*debugger-hook*` の実行中はスイッチを切るので入れ子にならない |
| Lisp が main の実行ファイル | 利用者が `handler-case` を書く。既定のトップレベルハンドラは置いていない |
| 深い再帰・スタック枯渇 | 上記のとおり `storage-condition` 化 |
| スレッド | ハンドラクラスタもリスタートもスレッドごと。ワーカースレッドには abort リスタートが無い |
| 割り込み | Ctrl+C は `INTERACTIVE-INTERRUPT` として配送する |

境界の実装側の規則が 2 つある。**内部の投機的な探索で signal しない**:
`dotnet:make-generic-type` は与えられた名前のまま引き、失敗したら
`` `N `` を足して引き直す、という当て推量をするが、その 1 回目が signal
していると、**呼び出し側が頼んでいない探索でユーザの `handler-bind` が
走る**。解決の本体は NIL を返す関数に分け、signal するのは本物の失敗だけに
してある。もう 1 つは **interop の隙間に裸の `catch` を置かない**こと。
dotcl は `return-from` の転送を例外として実装しているので、`catch { return
null; }` は非局所脱出を飲む。上の当て推量はこの 2 つを同時に踏んでいて、
「ハンドラは走ったのに脱出だけ消える」= テストが値は正しいのに abort 扱い、
という形で 1 年近く原因不明のまま残っていた。

**コールバック境界の既定は「透過」で、切り替えの設定は用意しない。**
ホスト側 (ASP.NET Core のような) は例外を受け取れば自分の作法で扱う —
実測では Lisp のエラーが 500 になり、サーバは生き続ける。ログして
コールバックだけ中断する、その場でデバッガに入る、といった選択肢を
設定として持たせることもできるが、それを必要とする具体的な用途が
まだ 1 つも出ていないため、契約を増やさない側に倒している。

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
高速パスを通る。Method dispatch は monomorphic inline cache
(1 entry) で primary / before / after / around を group し、
`CachedDispatch` でクラスタプル一致時にキャッシュ命中。EQL specializer
は `(eql X)` の cons で表現。クラス再定義は in-place 更新 + dependents
の re-finalize。

**AMOP 適合**: closer-mop の `features.lisp` が見る 95 項目のうち 94 を
通す (残り 1 は dotcl のメソッドラムダが spread 形で、AMOP の 2 引数形
ではないこと)。ここに至る過程で 2 種類の作業をした。

1 つ目は **metaobject を実体にする**こと。EQL specializer は `(eql x)` の
cons ではなく intern される `eql-specializer` オブジェクト (AMOP は EQL な
2 つに同じ metaobject を返せと言う = `eq` で比べられる必要がある)。
未定義の superclass の placeholder は `forward-referenced-class` を自分の
クラスとして返す。`(make-instance 'standard-class ...)` でクラスが作れる
(クラス生成が名前経由の `ensure-class` に寄っていて、この経路が無かった)。
`funcallable-standard-class` のインスタンスは呼べて
`set-funcallable-instance-function` が効く。利用者定義のメソッドクラスが
足したスロットも読める。

2 つ目は **プロトコル関数を総称関数に格上げ**すること
(`compute-slots` / `compute-applicable-methods-using-classes` /
`generic-function-method-class` / `compute-effective-method` /
`make-method-lambda` / `compute-discriminating-function`)。既定メソッドは
今の実装をそのまま呼ぶ。closer-mop は**プロトコル関数が総称関数である
ことを前提にしている** (`only-standard-methods` が渡された関数それぞれに
`generic-function-methods` を呼ぶ) ので、平の関数だとそこで落ちていた。

呼び出しの側をプロトコルに通すかは**ゲートの置き方が要点**。常に
`compute-applicable-methods` を通すとディスパッチキャッシュも arity 別の
直接デリゲートも失う。ゲートを「プロトコル関数に既定以外のメソッドがあるか」
にすると、**誰か一人が specialize した瞬間に image 内の全総称関数**が
包まれて確保が増える (一度これをやって確保テストが鳴った)。正しい条件は
「**この**総称関数に適用されるメソッドが既定以外か」で、静的 bool による
早期棄却と 2 段にしてある。

**ANSI / SBCL との差分**: ANSI 準拠。標準の metaobject は上記のとおり
実体を持つが、built-in クラスと構造体クラスは SBCL のようなメタクラス
階層ではなく `LispClass` の `IsBuiltIn` / `IsStructureClass` フラグで
区別する。dispatch cache は 4 エントリの inline cache (SBCL は
polymorphic inline cache + DAG)。Method combination は string registry
ベースで基本的なものだけ提供。

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
それ以外は `"PKG:NAME"` で qualified、ルックアップは qualified
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

**主要なファイル**: `runtime/Runtime.Thread.cs` (717 行、
`bt:make-thread` / `acquire-lock` / `condition-wait` 等の組み込み)。

**実装上の論点**: 親スレッドの動的束縛を `Snapshot()` して子で
`Restore()` (3.9 と連動)。`.NET Monitor` は再入可能なので、
`make-lock` と `make-recursive-lock` の差はフラグだけの semantic 区分。
`destroy-thread` は .NET 5+ で `Thread.Abort` が削除されているため
`Thread.Interrupt()` で代替する softer な実装にしている (destroy された
スレッドは restart 探索に落ちず黙って終了する)。`interrupt-thread` は
第 1 段として「.NET の待ち (lock / sleep / join / condition-wait) を
`Thread.Interrupt()` で叩き起こして割り込み thunk を配送する」実装が
入っている — 割り込みを飲み込むのは queue に配送物があるときだけ。
純計算ループ中の preemption は未対応で追跡継続。
グローバル状態のスレッドセーフ化は段階的に進めており、3.2 (Symbol
の volatile 化) も同じ流れ。ロックフリーな同期プリミティブとして
`atomic-long` (compare-and-swap / incf / decf、`Interlocked` ラップ) と
任意 CL place 用の汎用 atomic CAS を提供する。worker スレッドにも
top-level の `abort` リスタートを張るので、子スレッド内のエラーは
プロセスを巻き込まず個別に回収できる (lparallel 等が依存)。`eval` は
`dotcl:set-parallel-eval` で opt-in の並列評価に切り替えられ、`*macros*`
テーブルはそれに備えて synchronized。

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
ASDF 本体は別 fork (`github.com/dotcl/asdf`) の `dotcl-0.1.11` ブランチ
(互換世代 pin ブランチ) を `make setup-asdf` で取り込む。

**実装上の論点**: `.fasl` は cross-platform .NET IL なので Windows /
Linux / macOS の x86-64 / ARM64 で同じバイナリが動く。`.fasl` 0.73s
/ `.sil` 1.77s / `.lisp` 3.38s の load 時間差により ASDF のような
大物は `.fasl` 強制。同名モジュールの 2 重 load は
`_modulesLock` で防ぐ。`asdf/` 以下のソースを修正したら
`make setup-asdf` → `make compile-asdf-fasl` → `make pack` で
`.fasl` が再生成される。

**ビルド時の依存解決 (project-core)**: `<PackageReference DotCL.Runtime>`
+ `<DotclProjectAsd>` を持つ MSBuild プロジェクトでは、in-process の
ビルドタスク (`DotclHost.ResolveDeps` / `CompileProject`) が `.asd` の
`:depends-on` を辿り、依存システムを fasl 化して同梱、root を単一 fasl に
コンパイルしてマニフェストを書く。contrib 外の外部システムは
`<DotclAsdSearchPath>` (ディレクトリを `asdf:*central-registry*` に push)
または `<DotclBuildInit>` (resolve 前に走る Lisp スクリプト。`pushnew` や
quicklisp の boot 用の escape hatch) で明示的に discoverable にする。dotcl は
`~/quicklisp` 等を auto-scan せず `CL_SOURCE_REGISTRY` も読まない — ビルドを
再現可能に保つため依存の指定は宣言的 (env で成果物が揺れない)。ASDF が
コンパイルする fasl の出力先はプロジェクトの `obj/` 配下に向けており
(既定の共有ユーザキャッシュではなく)、`dotnet clean` で消える。これにより
再生成したソースを古いキャッシュ fasl が shadow する事故を防ぐ。

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
DLL を `Assembly.LoadFrom` で取り込む方式。`dotnet:resolve-type` は
ミス時に `AppContext.BaseDirectory` (配置済みアプリの PackageReference
アセンブリが並ぶ場所) の managed DLL を遅延ロードして再試行し、結果を
memoize、新規アセンブリ load で世代 invalidate する。これにより
PackageReference 型が手動 `load-assembly` なしで解決でき、生成コードから
`typeof(...).FullName` の強制ロードを撤去できた (samples/MonoGameLispDemo)。
CLOS dispatch では .NET 型ごとに `EnsureDotNetTypeClass` が built-in クラスを
get-or-register し、`class-of` / `typep` / defmethod 特定化子が機能する。
simple name (`Timer` 等) は最初に登録した型が取る早い者勝ちの対話用エイリアス
だが、FullName (`System.Threading.Timer`) は全型で必ず登録されるので、同名の
別型が居ても load 順に依存しない決定的な specializer になる。生成コードは
FullName specializer を焼けば常に正しく解決する。`dotnet:class-for-type` は
この登録クラスを型 (`System.Type` or 型名) から直接引く公開 API で、閉じた
ジェネリック型の長い assembly-qualified 名を綴らずに specializer を得られる。

**NuGet 依存の宣言と解決 (contrib `nuget` / `dotcl-nuget-asdf`)**:
`dotnet:require` (上記、nuget.org から直接 DL) とは別に、依存グラフごと
解決する経路がある。`nuget:require` は 1 パッケージだけを
`<PackageReference>` した使い捨て csproj に `dotnet build` を回し、
**版統一済みの推移閉包**を出力ディレクトリに平らに並べてから、managed /
RID 固有 native に分けてリゾルバに登録する (`project.assets.json` の形を
追うより、ディレクトリを走査する方が追従点が少ない)。結果の置き場は
4 段:

1. **実行ファイルの隣** (`<app-dir>/nuget/<key>/`) — `dotcl pack --bundle`
   が置く。あれば無条件にこれ。**指定が浮動版でも使う** (2 と規則が逆):
   浮動は「今いちばん新しいもの」だが、インストール済みのプログラムが
   それを確かめに行くべきではない
2. **固定版**ならユーザキャッシュ (`nuget:cache-root`、fasl キャッシュの
   兄弟に置いて「この OS でどこに書いてよいか」の答えを 2 つ持たない)
3. **浮動版**は使い捨て temp (「最新」は変わりうるので跨いで再利用しない)
4. 無ければ `dotnet build`。パッケージが NuGet のキャッシュに全部載って
   いても 1.5〜2 秒かかる (ダウンロードではなく MSBuild の起動)

宣言側は ASDF のコンポーネントクラス
(`(:nuget "Id" :nuget-version "13.0.3")`)。`:depends-on` の文法は閉じた
集合なので NuGet パッケージを綴る場所が無く、ASDF が用意している拡張点は
コンポーネントクラスの方 (cffi-grovel が `:cffi-grovel-file` で使うのと
同じ手)。**`:version` は使えない** — ASDF 自身のコンポーネント初期化引数で、
`defsystem` が横取りして自分のバージョン文法に通すため、固定版は通って
浮動版だけ黙って NIL に落ちるという最悪の形で壊れる。自前の
`:nuget-version` にし、`:version` が書かれていたら error にする。

「ファイルではないコンポーネント」は**出荷経路で 2 度消えた**: ASDF の
連結は `cl-source-file` しか拾わないので `dotcl pack` では宣言が落ち、
`dotcl build` は逆にコンポーネントのパス名を読もうとして落ちた。宣言を
ソースに戻す (`system-nuget-preamble` が `nuget:require` の呼び出しを
生成して連結単位の先頭に置く) ことで、出荷物が自分でパッケージを要求し、
同梱 layout の中でそれを見つける形にしてある。同梱は **RID ごとに解決して
RID ごとの bundle** に入れる — pack した機械の RID だけ入れると、残りの
パッケージは起動時に `dotnet build` に戻る空手形になる。

**ANSI / SBCL との差分**: ANSI 範囲外の dotcl 拡張。SBCL の CFFI が
foreign function call に閉じているのに対し、dotcl の `dotnet:` は
**CLR の同一型システム上で Lisp のクラスが定義される** ため、MAUI /
ASP.NET Core / MonoGame といったフレームワークがそのクラスを「ただの
.NET の型」として扱える (`samples/` の MauiLispDemo / AspNetLispDemo /
MonoGameLispDemo / McpServerDemo 参照)。

### 3.17 解釈経路 (emit-free)

**なぜ 2 つ目の評価器があるか**: `Reflection.Emit` が使えない実行環境が
ある。netstandard2.0 ターゲット (embedding 向け) と、IL2CPP / WebGL の
ような AOT codegen がそれで、そこでは「S 式 → CIL → JIT」の経路が丸ごと
成立しない。そのため木を直接歩く評価器 (`%mini-eval`) を持ち、コンパイラ
経路と同じ意味論を返すことを要求している。

**切り替え**: `dotcl:*evaluator-mode*` が `:compile` (既定) か
`:interpret`。ビルド側では `DotclEmit` プロパティが emit の有無を決め、
netstandard2.0 では常に false = 解釈経路だけになる。net10 ビルドで
`:interpret` を選べるのは、同じスイートを両経路に通して差分を取るため。

**ゲート**: `make test-regression-interp` が回帰スイートを解釈経路で
走らせる (コンパイル時の診断そのものを見るテストだけ opt out する)。
`make build-ns2` が emit-free 構成 (plain / JSON-free) をコンパイルする。
「ns2.0 はビルドが通る」だけでは評価器が一度も実行されないので、前者が本体。

**性能**: コンパイル経路と比べれば桁で遅い。自己末尾呼び出しは
トランポリンで畳んで深い再帰が溢れないようにしてある。ループ機構と
ノード単価の削減は継続中 (GitHub issues 参照)。

### 3.18 イメージ出力 (save-application)

`(dotcl:save-application path &key load prelude toplevel executable ...)`
で、ロード済みの定義を含む配布物を出す。中身は「入力ソースを
`compile-file` した FASL を束ねたもの」で、`:executable` を付けると
.NET の実行ファイルとして publish する (`:r2r` で ReadyToRun、RID 指定も
可能)。

**イメージ状態の再構築について**: SBCL の `save-lisp-and-die` のような
「実行時ヒープのダンプ」ではない。`defvar` / `defpackage` / リーダー
マクロなど、コンパイル対象ソースに書かれた定義は FASL の ModuleInit が
再評価するので自然に復元されるが、**実行時にだけ起きた状態変化は入らない**。
この線引きが `save-application` の設計そのもの。`Reflection.Emit` を要求
するので emit-free ビルドでは使えない (コンパイル済み FASL を配る側の道具)。

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
  contrib/     同梱モジュール 16 本 (nuget / dotcl-nuget-asdf /
               dotnet-class / dotnet-ffi / dotcl-thread / dotcl-socket /
               dotcl-kestrel / dotcl-gray / dotcl-repl / dotcl-lsp-api /
               advice / clrmd / decompiler / dotcl-cs / dotcl-float /
               dotcl-jitdisasm)。1 ディレクトリ 1 モジュールで、
               それぞれに README.md がある。`.asd` は require-system の
               スタブなので、ASDF 経由と `require` 経由が同じ経路に
               落ちる。ビルドが取り込む asdf/ と quicklisp/ もここに
               置かれる (生成物でリポジトリには入らない)
  samples/     dotcl を host する .NET 統合サンプル 8 本 (MauiLispDemo /
               AspNetLispDemo / MonoGameLispDemo / McpServerDemo /
               HotReloadDemo / PrecompiledLispDemo の 3 変種)。索引は
               samples/README.md
  examples/    Lisp スニペット集 (Windows interop など)
  docs/        トピック別のガイド (libraries.md / dotcl-pack.md /
               scripting.md / readytorun.md / windows.md ほか)。索引は
               docs/README.md
  test/        regression/ (dotcl 固有回帰テスト)、framework.lisp
  Makefile     build オーケストレーション
  README.md    ユーザ向け入口
  RELEASES.md  リリースノート
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
- **Step 6**: ASDF ロードと ansi-test 21,944 / 21,945 (99.99%) 達成
- **Step 7 (進行中)**: 最適化パス (型推論、unboxed 数値演算、
  インライン展開) — 命令リストに対するパスとして挟む A2 の利点を
  活かす。SBCL の IR1 を参考にしつつ CIL 向けの軽量 IR を別途設計。
  宣言駆動の native 整数演算・インライン展開・コールサイト inline cache は
  入っており、残りは GitHub issues
- **Step 8 (達成)**: セルフホスト + イメージ出力。Lisp 製コンパイラが
  dotcl 上で自身をコンパイルし、`dotcl:save-application` (3.18) で
  配布物を出せる。SBCL の `save-lisp-and-die` とは range が違う
  (実行時ヒープのダンプではない)。**世代の不動点**も機械で確認して
  いる: ツリーのコアが作ったコアが更に自分を再生産すること
  (`make selfhost-check`) と、**出荷済みの dotcl がこのツリーを
  建てられること** (`make seed-check`) の 2 つを CI が見る。後者が
  「ビルド鎖から Common Lisp ホストを外せる」条件で、既定のホストを
  切り替えるかどうかは別の判断として残してある
- **Step 8.5 (達成)**: 無改変 SBCL のクロスビルドホストとして通る
  (`./make.sh --xc-host=dotcl` が完走)。処理系としての網羅度を外部の
  巨大な CL コードで測る駆動源。残っているのは性能 (SBCL ホスト比)
- **Step 9 (部分達成)**: エディタ接続。Lem の swank fork である micros の
  dotcl backend が動き、upstream に提出済み (`lem-project/micros#22`)。
  backtrace / frame-locals / source-location / eval-in-frame / arglist
  などの中核は実装済みで、xref 呼出グラフや stepping は未実装。
  SLIME 本家の swank 側は未対応。プロトコルに依らない側は contrib
  `dotcl-lsp-api` に寄せてあり (カーソル位置の補完候補・リファレンス
  URL・その名前が何か)、同梱 REPL の TAB 補完も同じ口を使う
- **Step 10 (進行中)**: emit 無しで動く構成 (3.17)。netstandard2.0 /
  AOT 向けに、解釈経路とプリコンパイル済み FASL だけで CL を回す

直近 2 リリースがどこに進んだか (利用者向けの記述は RELEASES.md):

- **v0.1.27**: メタオブジェクトプロトコル (3.12)。closer-mop の
  `features.lisp` が見る 95 項目のうち 94 で conform、deviate 0 / error 0 —
  同じプローブで SBCL と並ぶ。このサイクルの開始時は 46 conforms /
  32 deviates / 16 errors だった。ほかにコマンドラインの契約 (綴りを
  間違えたフラグが黙って REPL にならず error になる)、スクリプトが自分の
  引数を読めること、NuGet の解決が SDK の無い機械でも効き続けること
- **v0.1.28**: load の速度 (3.8)。fasl のリテラルをデータ側に載せ、
  R2R 兄弟を読むようにして、coalton の load が 33 秒から 10 秒になった。
  ASDF システムが自分の NuGet 依存を宣言できるようになり (3.16)、
  エディタが生きた image にカーソル下の名前を訊けるようになった。
  規格側では FORMAT・パッケージ系・いくつかの列関数が、黙って通していた
  壊れた入力を signal するようになっている (移行時の注意は RELEASES.md)

未解決課題は GitHub Issues。

## 6. 技術的参考

- **ABCL** (JVM 上の Common Lisp): 独自コンパイラ + Java ランタイム。
  Lisp on managed runtime の先行例。
- **IronScheme** (.NET 上の Scheme): C# でランタイム、DLR 活用。
- **System.Reflection.Emit**: `DynamicMethod` (軽量、GC 回収可能) と
  `PersistedAssemblyBuilder` (.dll 出力可能、.NET 9+ で復活) の 2 モード。
- **MIT LOOP** (Symbolics / Glenn Burke 系): `compiler/loop.lisp` の
  出自。

## 7. 関連リソース

- 公開 issue / PR: <https://github.com/dotcl/dotcl/issues>
- リリースノート: `RELEASES.md`
- 本ファイルは機構別の実装ノート。設計判断や時系列の作業履歴は
  内部リポジトリで管理している。
