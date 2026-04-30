# Releases

User-facing release notes for dotcl. Each section corresponds to a tagged
release on the public mirror (dotcl/dotcl).

## v0.1.1 — 2026-04-30

RID expansion release.

### Changed

- **R2R AOT FASLs now ship for win-x64, win-arm64, linux-x64, linux-arm64,
  osx-x64, osx-arm64.** v0.1.0 only had R2R for win-arm64; other
  platforms used the framework-dependent FASL (slower cold start).
- Added per-RID nupkgs: `dotcl.win-x64`, `dotcl.linux-x64`,
  `dotcl.linux-arm64`, `dotcl.osx-x64`, `dotcl.osx-arm64`.

### Notes

- 32-bit builds (win-x86) intentionally not shipped.
- crossgen2 cross-compile is used; FASLs for non-host RIDs are produced
  on the win-arm64 dev machine.

## v0.1.0 — 2026-04-29

Initial public release.

### Highlights

- Common Lisp implementation on .NET 10. Lisp source is compiled to CIL
  and runs on the .NET JIT — same Lisp image runs on Windows, macOS, and
  Linux across x86-64 and ARM64.
- Broadly conforms to the ANSI Common Lisp standard (verified against
  the [ansi-test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test)).
- `dotnet:` package for .NET interop: instantiate types, invoke methods,
  subclass via `dotnet:define-class` (real .NET classes emitted from
  Lisp; frameworks like MAUI / ASP.NET Core / MonoGame see them as
  ordinary subclasses).
- ASDF support (forked + adapted, ships as IL-only `.fasl` for
  cross-platform load).
- Working sample integrations in `samples/`: ASP.NET Core, MAUI,
  MonoGame, MCP server.

### Known limitations

- Single-image model (load-time codegen). No separate compilation per
  file in the traditional Lisp sense.
- Multi-platform R2R AOT currently only available for `win-arm64`.

### License

MIT. See [`LICENSE`](LICENSE).
