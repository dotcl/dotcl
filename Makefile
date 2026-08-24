DOTCL_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
# Cross-compile host. Pin SBCL via Roswell so the build doesn't silently use
# whatever `ros` default the user happens to have (installing another impl, e.g.
# ABCL, flips the default and breaks cross-compile, dotcl/dotcl #35). Still
# overridable, e.g. `DOTCL_LISP=dotcl make cross-compile` to self-host.
DOTCL_LISP ?= ros -L sbcl-bin run
STDBUF ?=
SETSID ?= $(shell which setsid 2>/dev/null)

.PHONY: all build build-ns2 check-contrib-freshness run clean repl test-fasl-shape test-core-bytes test-host-api test-coverage test-ansi-all test-ansi-full test-ansi-extra test-regression test-regression-interp test-regression-emitfree test-pack-nuspec test-save-class-lib test-project-compose test-project-core-build test-mop ilverify update-ansi-state commit-ansi-state cross-compile selfhost-check selfhost-test seed-install seed-check loc publish pack install setup-ansi-test setup-asdf setup-quicklisp setup-cl-bench bench bench-state bench-survey compile-asdf-fasl compile-asdf-fasls compile-quicklisp-fasl compile-core-fasl compile-contrib-fasls contrib-dotcl-cs contrib-dotcl-jitdisasm gen-char-names

# Source files for cross-compile. Listed once; the recipe and dependency
# tracking both reference this so adding a file is a single-edit change.
CIL_SOURCES := \
  $(DOTCL_ROOT)compiler/cil-compiler.lisp \
  $(DOTCL_ROOT)compiler/cil-stdlib.lisp \
  $(DOTCL_ROOT)compiler/cil-macros.lisp \
  $(DOTCL_ROOT)compiler/loop.lisp \
  $(DOTCL_ROOT)compiler/cil-analysis.lisp \
  $(DOTCL_ROOT)compiler/cil-forms.lisp

# Runtime (C#) sources. A compiled .fasl carries the code generation of the
# RUNTIME that produced it as much as the compiler's: the emitter lives here, in
# C#. cil-out.sil does not move when only C# changes, so a fasl built before such
# a change stays "up to date" by the .sil dependency alone and keeps its old
# codegen -- which is how an emitter change that broke loading asdf passed
# locally and failed in CI, where every run rebuilds the fasl.
RUNTIME_SOURCES := \
  $(wildcard $(DOTCL_ROOT)runtime/*.cs) \
  $(wildcard $(DOTCL_ROOT)runtime/Emitter/*.cs) \
  $(wildcard $(DOTCL_ROOT)runtime/Diagnostics/*.cs) \
  $(wildcard $(DOTCL_ROOT)runtime/Generated/*.cs)

all: cross-compile build

build: $(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs
	dotnet build $(DOTCL_ROOT)runtime/runtime.csproj

# Diagnostic for "I fixed the compiler but the fix does not take effect".
#
# A prebuilt contrib fasl carries the code generation of the compiler that built
# it, so one produced before a codegen change keeps the OLD behaviour even
# though cil-out.sil has the fix. Run this when a change seems inert; it reports
# two independent problems.
#
# Deliberately NOT wired into `build`: contrib fasls are older than cil-out.sil
# after every compiler edit, so warning there would fire constantly and be
# tuned out. The shadow report below is the rare, always-actionable one.
#
# Regenerate with: make compile-asdf-fasl compile-contrib-fasls
check-contrib-freshness:
	@sil=$(DOTCL_ROOT)compiler/cil-out.sil; \
	if [ -f "$$sil" ]; then \
	  stale=""; \
	  for f in $(DOTCL_ROOT)contrib/*/*.fasl; do \
	    case "$$f" in *-r2r-*) continue;; esac; \
	    [ -f "$$f" ] || continue; \
	    [ "$$f" -ot "$$sil" ] && stale="$$stale $$f"; \
	  done; \
	  if [ -n "$$stale" ]; then \
	    echo "older than compiler/cil-out.sil (built by an older compiler):"; \
	    for f in $$stale; do echo "  $$f"; done; \
	  else \
	    echo "contrib fasls are newer than cil-out.sil"; \
	  fi; \
	fi
	@# The same question against the RUNTIME sources. The emitter is C#, so a
	@# fasl can be newer than cil-out.sil and still carry superseded code
	@# generation. Reported separately because the answer differs: a compiler
	@# edit makes every fasl "old" and is usually irrelevant, while a C# emitter
	@# edit is exactly the case that produced a green local run and a red CI one.
	@newest=$$(ls -t $(RUNTIME_SOURCES) 2>/dev/null | head -1); \
	if [ -n "$$newest" ]; then \
	  stale=""; \
	  for f in $(DOTCL_ROOT)contrib/*/*.fasl $(DOTCL_ROOT)compiler/dotcl.core; do \
	    case "$$f" in *-r2r-*) continue;; esac; \
	    [ -f "$$f" ] || continue; \
	    [ "$$f" -ot "$$newest" ] && stale="$$stale $$f"; \
	  done; \
	  if [ -n "$$stale" ]; then \
	    echo "older than $$(basename $$newest) (built by an older RUNTIME):"; \
	    for f in $$stale; do echo "  $$f"; done; \
	  else \
	    echo "contrib fasls are newer than the runtime sources"; \
	  fi; \
	fi
	@echo ""
	@# Which file each (require "<name>") actually reaches, and what it hides.
	@# Comparing timestamps against cil-out.sil (above) says "old" about
	@# everything after any compiler edit; this says which single candidate wins,
	@# which is the part that explains an edit not taking effect.
	@bash $(DOTCL_ROOT)scripts/contrib-resolve.sh $(DOTCL_ROOT)

run:
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj

repl:
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --repl

# The suite loads asdf, so it exercises whatever asdf.fasl holds. $(wildcard)
# rather than a plain prerequisite: a tree that has no fasl yet must not be made
# to clone and build asdf just to run the suite, but one that HAS a fasl must not
# run against a stale one. Without this the suite reports on the codegen of
# whenever the fasl was last built -- green locally, red in CI, which is exactly
# how a broken emitter change once shipped.
test-regression: build $(DOTCL_ROOT)compiler/cil-out.sil $(wildcard $(DOTCL_ROOT)contrib/asdf/asdf.fasl)
	@echo "=== Running dotcl regression tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/regression/run.lisp

# The same suite with EVAL routed through the emit-free tree-walk interpreter.
# That interpreter is the ONLY evaluator on netstandard2.0 (AOT/WebGL) builds,
# and `build-ns2` merely compiles that runtime — it runs no tests, so until this
# target existed the evaluator those builds depend on had never been executed by
# CI at all. Tests asserting a compile-time diagnostic (a warning, the IL size
# limit, refusal to generate code) opt out via DEFTEST-COMPILED-ONLY.
#
# Scope, so this is not read as more than it is: *evaluator-mode* redirects EVAL
# only — LOAD still compiles top-level forms. This exercises the interpreter
# wherever a test reaches it through EVAL, which is far better than nothing but
# is not the same as running the suite ON an emit-free build.
test-regression-interp: build $(DOTCL_ROOT)compiler/cil-out.sil $(wildcard $(DOTCL_ROOT)contrib/asdf/asdf.fasl)
	@echo "=== Running dotcl regression tests (tree-walk interpreter) ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(setq dotcl:*evaluator-mode* :interpret)' $(DOTCL_ROOT)test/regression/run.lisp

# The same suite ON an emit-free build — no System.Reflection.Emit anywhere, so
# the tree-walk interpreter is the only evaluator and even LOAD of a .lisp goes
# through it. This is what `test-regression-interp` above is only a proxy for:
# there, LOAD still compiles each top-level form, so an interpreted DEFUN never
# exists. The interpreted-DEFUN declaration bug was found here and cannot be
# found there.
#
# NOT wired into CI yet: it currently stops at test/regression/macroexpand-hook.lisp
# with a stack overflow — an interpreted *MACROEXPAND-HOOK* re-enters, because the
# interpreter macroexpands the hook's own body on every call. Tracked separately.
#
# -p:DotclNoEmit=true flips the DOTCL_EMIT constant off for an ordinary desktop
# framework (see runtime/DotCL.Runtime.csproj), so this differs from a normal run
# in exactly one axis. It needs the FASL core: --asm of a .sil would itself want
# the assembler.
test-regression-emitfree: $(DOTCL_ROOT)compiler/dotcl.core $(wildcard $(DOTCL_ROOT)contrib/asdf/asdf.fasl)
	@echo "=== Running dotcl regression tests (emit-free build) ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -p:DotclNoEmit=true -- --core $(DOTCL_ROOT)compiler/dotcl.core $(DOTCL_ROOT)test/regression/run.lisp

test-debug-pdb: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running debug-path (DOTCL_EMIT_PDB) checks ==="
	sh $(DOTCL_ROOT)test/debug-pdb/check.sh $(DOTCL_ROOT)

# Line coverage for .lisp via an off-the-shelf .NET coverage collector: the PDB
# names the .lisp, so no coverage-specific code of ours is involved. Skips
# cleanly when the dotnet-coverage tool is absent.
test-coverage: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running .lisp line-coverage checks ==="
	sh $(DOTCL_ROOT)test/coverage/check.sh $(DOTCL_ROOT)

# Asserts a packed tool's nuspec describes the app, not dotcl. Needs published
# dotcl packages (`make pack`); skips cleanly when they are absent.
test-pack-nuspec: build
	@echo "=== Running pack nuspec checks ==="
	sh $(DOTCL_ROOT)test/pack-nuspec/check.sh $(DOTCL_ROOT)

# Asserts dotcl can emit a .NET DLL that a separate C# app references at compile
# time (save-class-library stage 1). Round-trips via dotnet build; needs .NET 9+.
test-save-class-lib: build
	@echo "=== Running save-class-library checks ==="
	sh $(DOTCL_ROOT)test/save-class-lib/check.sh $(DOTCL_ROOT)


# The MSBuild integration users actually get: the ProjectCore targets driving the
# in-process tasks. test-project-compose builds with the legacy targets (which
# shell out), so nothing else here executes the task assembly at all. Assembles
# the package layout the targets resolve against and builds a project twice: once
# with a dependency that cannot be found (the message has to name the remedy) and
# once with DotclAsdSearchPath set (it has to build).
test-project-core-build: build $(DOTCL_ROOT)compiler/dotcl.core
	@echo "=== Running project-core MSBuild task checks ==="
	sh $(DOTCL_ROOT)test/project-core-build/check.sh $(DOTCL_ROOT)

test-project-compose: build
	@echo "=== Running project-core composition checks ==="
	sh $(DOTCL_ROOT)test/project-compose/check.sh $(DOTCL_ROOT)

# Booting from a core held in memory (DotclHost.LoadCore(byte[])) — the only way
# in for a host with no filesystem, and for the emit-free runtime the only way in
# at all. Covers both the normal and the emit-free build.
test-host-api: build $(DOTCL_ROOT)compiler/dotcl.core
	@echo "=== Running embedding-API checks ==="
	sh $(DOTCL_ROOT)test/host-api/check.sh $(DOTCL_ROOT)

test-core-bytes: build $(DOTCL_ROOT)compiler/dotcl.core
	@echo "=== Running in-memory core checks ==="
	sh $(DOTCL_ROOT)test/core-bytes/check.sh $(DOTCL_ROOT)

test-ansi-extra: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running CLHS audit extra tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/test-ansi-extra.lisp

test-mop: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running AMOP protocol conformance tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/mop-protocol.lisp

# Assert the emitter produces VERIFIABLE CIL (no covariant calls / stack-type
# mismatches). Unverifiable IL runs on CoreCLR but is rejected by strict AOT C++
# codegens (Unity IL2CPP / WebGL). Catches such codegen regressions in seconds
# instead of via a 25-minute IL2CPP build. Needs dotnet-ilverify (the target
# prints the install command if missing).
ilverify: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Verifying emitted CIL (ilverify) ==="
	bash $(DOTCL_ROOT)scripts/ilverify-check.sh

# ilverify's companion: it asks whether the IL is VALID, this asks whether the
# assembly has a loadable SHAPE — no single method too large to JIT cheaply, no
# type near the field limit, no oversized #US heap. Those failures are invisible
# on the compile side (total IL, fasl bytes and compile time stay flat) and land
# at LOAD as a diagnostic that names neither file nor cause. Every instance so
# far was found by a user running out of memory rather than by CI.
test-fasl-shape: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Checking fasl shape (per-method IL / fields per type / #US heap) ==="
	bash $(DOTCL_ROOT)scripts/fasl-shape-check.sh

# Compile-only tripwire for the netstandard2.0 runtime — the build that AOT
# (NativeAOT) and WebGL (Unity IL2CPP) link against. `make build` only compiles
# the dev net10 runner, so an unguarded Reflection.Emit use (absent on ns2.0) or
# a broken DOTCL_NO_JSON #if would otherwise surface only in a heavy AOT/IL2CPP
# build. This catches it in seconds. Builds both shipped ns2.0 configs: plain
# (with System.Text.Json) and emit-free/JSON-free (what the shippable samples use).
build-ns2:
	@echo "=== Building netstandard2.0 runtime (plain) ==="
	dotnet build $(DOTCL_ROOT)runtime/DotCL.Runtime.csproj -c Release -f netstandard2.0
	@echo "=== Building netstandard2.0 runtime (emit-free, JSON-free) ==="
	dotnet build $(DOTCL_ROOT)runtime/DotCL.Runtime.csproj -c Release -f netstandard2.0 -p:DotclNoJson=true
	@# net8.0 is the third shipped target framework and the only one that keeps the
	@# emitter sources while lacking PersistedAssemblyBuilder (.NET 9+), so a field
	@# assigned only by the .fasl writer reads as never-assigned there. With
	@# warnings promoted to errors that is a build failure, and it used to surface
	@# for the first time inside `pack` — during a release. Build it here instead.
	@echo "=== Building net8.0 runtime (embeddable; emitter present, no fasl writer) ==="
	dotnet build $(DOTCL_ROOT)runtime/DotCL.Runtime.csproj -c Release -f net8.0

test-ansi-full: build setup-ansi-test
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/test-ansi.lisp

ANSI_CATEGORIES := symbols eval-and-compile data-and-control-flow iteration \
	objects conditions cons arrays hash-tables packages numbers sequences \
	structures types-and-classes strings characters pathnames files \
	streams printer reader system-construction environment misc

test-ansi-all: build setup-ansi-test
	@total_pass=0; total_fail=0; total_tests=0; total_alloc=0; total_gen0=0; total_gen1=0; total_gen2=0; \
	for cat in $(ANSI_CATEGORIES); do \
		tmp=$$(mktemp /tmp/dotcl-ansi-XXXXXX.lisp); \
		cat $(DOTCL_ROOT)test/test-ansi-cat.lisp > $$tmp; \
		echo "(load \"ansi-test/$$cat/load.lsp\")" >> $$tmp; \
		echo "(let ((s0 (dotcl:gc-stats)))" >> $$tmp; \
		echo "  (let ((*load-pathname* nil) (*load-truename* nil)) (rt:do-tests))" >> $$tmp; \
		echo "  (let ((s1 (dotcl:gc-stats)))" >> $$tmp; \
		echo "    (format t \"~&;GCSTATS gen0=~D gen1=~D gen2=~D alloc=~D~%\"" >> $$tmp; \
		echo "            (- (nth 0 s1) (nth 0 s0))" >> $$tmp; \
		echo "            (- (nth 1 s1) (nth 1 s0))" >> $$tmp; \
		echo "            (- (nth 2 s1) (nth 2 s0))" >> $$tmp; \
		echo "            (- (nth 4 s1) (nth 4 s0)))))" >> $$tmp; \
		t0=$$(date +%s); \
		outfile=/tmp/ansi-$$cat.txt; \
		$(STDBUF) $(SETSID) timeout 360 dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$tmp > $$outfile 2>&1; \
		exitcode=$$?; \
		t1=$$(date +%s); \
		elapsed=$$((t1 - t0)); \
		gc_line=$$(grep -a '^;GCSTATS ' $$outfile | tail -1); \
		gen0=$$(echo "$$gc_line" | sed -n 's/.*gen0=\([0-9]*\).*/\1/p'); \
		gen1=$$(echo "$$gc_line" | sed -n 's/.*gen1=\([0-9]*\).*/\1/p'); \
		gen2=$$(echo "$$gc_line" | sed -n 's/.*gen2=\([0-9]*\).*/\1/p'); \
		alloc=$$(echo "$$gc_line" | sed -n 's/.*alloc=\([0-9]*\).*/\1/p'); \
		alloc_mb=$$([ -n "$$alloc" ] && echo $$((alloc / 1048576)) || echo "?"); \
		[ -n "$$alloc" ] && total_alloc=$$((total_alloc + alloc)); \
		[ -n "$$gen0" ] && total_gen0=$$((total_gen0 + gen0)); \
		[ -n "$$gen1" ] && total_gen1=$$((total_gen1 + gen1)); \
		[ -n "$$gen2" ] && total_gen2=$$((total_gen2 + gen2)); \
		if [ $$exitcode -eq 124 ]; then \
			printf "%-25s TIMEOUT (%ds)  -> %s\n" "$$cat:" $$elapsed $$outfile; \
			rm -f $$tmp; \
			continue; \
		fi; \
		total_line=$$(grep -a 'tests total' $$outfile); \
		total=$$(echo "$$total_line" | grep -o '[0-9]* tests total' | awk '{print $$1}'); \
		fail_line=$$(grep -a 'out of .* total tests failed' $$outfile); \
		no_fail=$$(grep -a 'No tests failed' $$outfile); \
		if [ -n "$$fail_line" ]; then \
			fail=$$(echo "$$fail_line" | awk '{print $$1}'); \
			pass=$$((total - fail)); \
			printf "%-25s %5d/%5d pass  (%ds, %sMB alloc)  -> %s\n" "$$cat:" $$pass $$total $$elapsed $$alloc_mb $$outfile; \
			total_pass=$$((total_pass + pass)); \
			total_fail=$$((total_fail + fail)); \
			total_tests=$$((total_tests + total)); \
		elif [ -n "$$no_fail" ]; then \
			printf "%-25s %5d/%5d pass  (%ds, %sMB alloc)  -> %s\n" "$$cat:" $$total $$total $$elapsed $$alloc_mb $$outfile; \
			total_pass=$$((total_pass + total)); \
			total_tests=$$((total_tests + total)); \
		elif [ -n "$$total" ]; then \
			printf "%-25s CRASH (%ds, %d tests loaded)  -> %s\n" "$$cat:" $$elapsed $$total $$outfile; \
			total_tests=$$((total_tests + total)); \
		else \
			printf "%-25s CRASH (%ds)  -> %s\n" "$$cat:" $$elapsed $$outfile; \
		fi; \
		rm -f $$tmp; \
	done; \
	echo ""; \
	printf "%-25s %5d/%5d pass (%d failures)\n" "TOTAL:" $$total_pass $$total_tests $$total_fail; \
	printf "%-25s gen0=%d gen1=%d gen2=%d alloc=%dMB\n" "GC TOTAL:" $$total_gen0 $$total_gen1 $$total_gen2 $$((total_alloc / 1048576))

update-ansi-state:
	@has_results=0; \
	for cat in $(ANSI_CATEGORIES); do \
		if [ -f "/tmp/ansi-$$cat.txt" ]; then has_results=1; break; fi; \
	done; \
	if [ $$has_results -eq 0 ]; then \
		echo "No /tmp/ansi-*.txt results found; keeping existing ansi-state.json"; \
	else \
		: 'Capture the hand-written half of the file BEFORE the redirection below'; \
		: 'truncates it. Everything between "source" and "categories" is judgement'; \
		: 'no run can reproduce -- which failures are known and why, which tests a'; \
		: 'note excludes and what three implementations answered on the same forms'; \
		: '-- and regenerating without it silently deleted all of it, with'; \
		: 'commit-ansi-state standing by to commit the deletion.'; \
		curated=""; \
		if [ -f $(DOTCL_ROOT)ansi-state.json ]; then \
			curated=$$(sed -n '/^  "source"/,/^  "categories"/p' $(DOTCL_ROOT)ansi-state.json | sed '$$d'); \
		fi; \
		{ \
		echo '{'; \
		echo '  "updated": "'"$$(date +%Y-%m-%d)"'",'; \
		if [ -n "$$curated" ]; then printf '%s\n' "$$curated"; fi; \
		completed=""; \
		for cat in $(ANSI_CATEGORIES); do \
			outfile=/tmp/ansi-$$cat.txt; \
			if [ -f "$$outfile" ] && strings "$$outfile" | grep -q 'No tests failed'; then \
				total=$$(strings "$$outfile" | grep -o '[0-9]* tests total' | awk '{print $$1}'); \
				if [ -n "$$total" ] && [ "$$total" -gt 0 ] 2>/dev/null; then \
					completed="$$completed \"$$cat\","; \
				fi; \
			fi; \
		done; \
		completed=$$(echo "$$completed" | sed 's/,$$//'); \
		echo "  \"completed\": [$$completed],"; \
		echo '  "categories": {'; \
		first=1; \
		for cat in $(ANSI_CATEGORIES); do \
			outfile=/tmp/ansi-$$cat.txt; \
			if [ $$first -eq 0 ]; then echo ','; fi; first=0; \
			if [ ! -f "$$outfile" ]; then \
				printf '    %-30s {"tests": null, "pass": null, "status": "untested", "blocker": null}' "\"$$cat\":"; \
				continue; \
			fi; \
			total=$$(strings "$$outfile" | grep -o '[0-9]* tests total' | awk '{print $$1}'); \
			fail_line=$$(strings "$$outfile" | grep 'out of .* total tests failed'); \
			no_fail=$$(strings "$$outfile" | grep 'No tests failed'); \
			if [ -z "$$total" ]; then \
				blocker=$$(tail -5 "$$outfile" | head -1 | sed 's/"/\\"/g' | cut -c1-80); \
				printf '    %-30s {"tests": null, "pass": null, "status": "blocked", "blocker": "%s"}' "\"$$cat\":" "$$blocker"; \
			elif [ -n "$$no_fail" ]; then \
				printf '    %-30s {"tests": %s, "pass": %s, "status": "complete", "blocker": null}' "\"$$cat\":" "$$total" "$$total"; \
			elif [ -n "$$fail_line" ]; then \
				fail=$$(echo "$$fail_line" | awk '{print $$1}'); \
				pass=$$((total - fail)); \
				printf '    %-30s {"tests": %s, "pass": %s, "status": "ready", "blocker": null}' "\"$$cat\":" "$$total" "$$pass"; \
			else \
				blocker=$$(tail -5 "$$outfile" | head -1 | sed 's/"/\\"/g' | cut -c1-80); \
				printf '    %-30s {"tests": %s, "pass": null, "status": "blocked", "blocker": "%s"}' "\"$$cat\":" "$$total" "$$blocker"; \
			fi; \
		done; \
		echo ''; \
		echo '  }'; \
		echo '}'; \
		} > $(DOTCL_ROOT)ansi-state.json; \
		echo "Updated ansi-state.json"; \
		cat $(DOTCL_ROOT)ansi-state.json; \
	fi

commit-ansi-state: update-ansi-state
	git add -f $(DOTCL_ROOT)ansi-state.json
	git commit -m "Update ansi-state.json"

setup-ansi-test:
	@if [ ! -d $(DOTCL_ROOT)ansi-test ]; then \
		echo "Cloning ansi-test (gitlab.common-lisp.net)..."; \
		git clone https://gitlab.common-lisp.net/ansi-test/ansi-test.git $(DOTCL_ROOT)ansi-test; \
	else \
		echo "ansi-test/ already exists"; \
	fi

# asdf/ and quicklisp-client/ are gitignored working copies, so a checkout left
# on the wrong branch is invisible to git status and survives git reset --hard.
# Which branch a tree ends up on is decided by when it was first cloned, so two
# worktrees of the same repository can disagree. Name the expected branch here
# and check it; override on the command line to build against another one.
ASDF_BRANCH ?= dotcl-0.1.21
QUICKLISP_CLIENT_BRANCH ?= dotcl-support

setup-asdf:
	@# dotcl-0.1.21 is the compat-generation bundle branch: it pairs with the
	@# launch-process keyword API, the run-time os-cond / single-FASL work, and
	@# the uiop #+dotcl backends (env writes, chdir, hostname,
	@# delete-empty-directory, run-program :error-output :output, combine-fasls)
	@# that need runtime primitives shipped in 0.1.21. A new dotcl-X.Y.Z branch is
	@# cut on a hard #+dotcl incompatibility (the previous dotcl-0.1.11 stays frozen
	@# for the 0.1.11-era runtime). The old `dotcl` branch stays frozen so
	@# pre-0.1.11 source builds keep cloning a matching asdf.
	@if [ ! -d $(DOTCL_ROOT)asdf ]; then \
		echo "Cloning asdf..."; \
		git clone --branch $(ASDF_BRANCH) https://github.com/dotcl/asdf.git $(DOTCL_ROOT)asdf; \
	else \
		cur=$$(git -C $(DOTCL_ROOT)asdf rev-parse --abbrev-ref HEAD 2>/dev/null); \
		if [ -z "$$cur" ]; then \
			echo "asdf/: not a git checkout, cannot verify it is $(ASDF_BRANCH)"; \
		elif [ "$$cur" != "$(ASDF_BRANCH)" ]; then \
			echo "asdf/ is on '$$cur', but this tree builds against '$(ASDF_BRANCH)'."; \
			echo "  git -C $(DOTCL_ROOT)asdf fetch origin"; \
			echo "  git -C $(DOTCL_ROOT)asdf switch $(ASDF_BRANCH)"; \
			echo "If '$$cur' is deliberate, re-run with ASDF_BRANCH=$$cur."; \
			exit 1; \
		fi; \
	fi
	@# Unconditionally, not only when build/asdf.lisp is missing: make-asdf.sh
	@# concatenates to a .tmp and cmp-and-moves, so unchanged source leaves the
	@# file (and its mtime) alone. Guarding on existence instead made a branch
	@# switch invisible — the concatenation from the old branch stayed behind and
	@# kept being copied on to contrib/, so the rebuilt fasl was still the old one.
	@cd $(DOTCL_ROOT)asdf && sh make-asdf.sh
	@mkdir -p $(DOTCL_ROOT)contrib/asdf
	@# cmp-then-cp so unchanged source doesn't bump dest mtime (which would
	@# cascade-rebuild asdf.fasl unnecessarily on every compile-asdf-fasl call).
	@cmp -s $(DOTCL_ROOT)asdf/build/asdf.lisp $(DOTCL_ROOT)contrib/asdf/asdf.lisp 2>/dev/null \
	  || cp $(DOTCL_ROOT)asdf/build/asdf.lisp $(DOTCL_ROOT)contrib/asdf/asdf.lisp

setup-quicklisp:
	@# dotcl-support is both the branch submitted upstream (quicklisp-client PR
	@# "Add dotcl (Common Lisp on .NET) support") and the shipping branch. Unlike
	@# asdf there is no per-generation branch: the dotcl surface is 3 definterface
	@# implementations (socket / init-file-name / directory-entries) and does not
	@# grow with OS or runtime features, so one branch stays accurate. If a commit
	@# ever has to ship that cannot go into the PR, split the two and add a CI
	@# check that the shipping branch contains the PR branch.
	@if [ ! -d $(DOTCL_ROOT)quicklisp-client ]; then \
		echo "Cloning quicklisp-client..."; \
		git clone --branch $(QUICKLISP_CLIENT_BRANCH) https://github.com/dotcl/quicklisp-client.git $(DOTCL_ROOT)quicklisp-client; \
	else \
		cur=$$(git -C $(DOTCL_ROOT)quicklisp-client rev-parse --abbrev-ref HEAD 2>/dev/null); \
		if [ -z "$$cur" ]; then \
			echo "quicklisp-client/: not a git checkout, cannot verify it is $(QUICKLISP_CLIENT_BRANCH)"; \
		elif [ "$$cur" != "$(QUICKLISP_CLIENT_BRANCH)" ]; then \
			echo "quicklisp-client/ is on '$$cur', but this tree builds against '$(QUICKLISP_CLIENT_BRANCH)'."; \
			echo "  git -C $(DOTCL_ROOT)quicklisp-client fetch origin"; \
			echo "  git -C $(DOTCL_ROOT)quicklisp-client switch $(QUICKLISP_CLIENT_BRANCH)"; \
			echo "If '$$cur' is deliberate, re-run with QUICKLISP_CLIENT_BRANCH=$$cur."; \
			exit 1; \
		fi; \
	fi
	@mkdir -p $(DOTCL_ROOT)contrib/quicklisp
	@sh $(DOTCL_ROOT)scripts/build-quicklisp.sh $(DOTCL_ROOT)quicklisp-client $(DOTCL_ROOT)contrib/quicklisp/quicklisp.lisp

# Benchmarks: make bench / make bench SUITE=gabriel / make bench BENCH=tak
SUITE ?=
BENCH ?=
BENCH_TIMEOUT ?= 600

# Benchmarks measure the RELEASE build. `dotnet run --project` builds Debug, and
# a Debug assembly is a different implementation to the JIT: it skips the
# optimisations that depend on the C# compiler's output shape. Sealing the core
# object types measured 0% through `dotnet run` and -17% on the same
# machine in Release, because the JIT does not devirtualise `is Cons` in a
# debuggable assembly. Allocation numbers (the /consed columns) do not depend on
# the configuration, but every time figure here does.
#
# The exe is invoked directly rather than through `dotnet run` for the same
# reason the profile is collected that way: `dotnet run` also charges its own
# build check and launcher to the first measurement.
# Recursive (=), not simple (:=): HOST_RID is defined further down this file, so
# a simply-expanded assignment here would see it empty and always pick the
# POSIX name.
BENCH_EXE_NAME = $(if $(filter win-%,$(HOST_RID)),runtime.exe,runtime)
BENCH_RUNTIME  = $(DOTCL_ROOT)runtime/bin/Release/net10.0/$(BENCH_EXE_NAME)

.PHONY: bench-build
bench-build:
	@dotnet build $(DOTCL_ROOT)runtime/runtime.csproj -c Release -v:q --nologo

# The harness ships, but a tree can still be missing it (a source tarball that
# dropped bench/, say), and then these targets have no inputs. Say so and stop,
# instead of failing partway through on a missing file. The check is a
# parse-time $(wildcard) rather than a `test -d` guard in the recipe, because
# each recipe line is its own shell — an `exit 0` on the first line would not
# stop the remaining ones. It is also safe to glob at parse time: bench/run.lisp
# is a checked-in file, not something an earlier recipe line produces. Relative
# path: this make always runs in the repo root, and a native make would not
# resolve the POSIX form of $(DOTCL_ROOT).
ifeq ($(wildcard bench/run.lisp),)

bench bench-state bench-survey:
	@echo "$@: bench/run.lisp not found — the benchmark harness is not present in this tree."

else

bench: setup-cl-bench bench-build
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) $(BENCH_RUNTIME) --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; TIMEOUT after $(BENCH_TIMEOUT)s"; fi

# Generate bench-state.json with dotcl and SBCL results side by side
bench-state: setup-cl-bench bench-build
	@echo "=== Running benchmarks on dotcl ==="
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) $(BENCH_RUNTIME) --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-dotcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; dotcl TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@echo "=== Running benchmarks on SBCL ==="
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval timeout $(BENCH_TIMEOUT) $(SBCL_RUN) $$EVAL_ARGS --load $(DOTCL_ROOT)bench/run.lisp --eval "'(quit)'" 2>/tmp/bench-sbcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; SBCL TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@$(DOTCL_ROOT)bench/make-state.sh /tmp/bench-dotcl.txt /tmp/bench-sbcl.txt $(DOTCL_ROOT)bench-state.json > /tmp/bench-state-new.json && mv /tmp/bench-state-new.json $(DOTCL_ROOT)bench-state.json
	@echo "Updated bench-state.json"
	@cat $(DOTCL_ROOT)bench-state.json


# Survey mode: run each bench N times, record median/min/max/stddev/cv.
#   make bench-survey [SUITE=...] [BENCH=...] [RUNS=5] [WARMUP=1]
# Merges into bench-state.json as dotcl_stats / sbcl_stats fields,
# while keeping top-level dotcl/sbcl/ratio pointing at the median for
# backward compatibility with the coordinator prompt.
RUNS ?= 5
WARMUP ?= 1
# Pin SBCL (see DOTCL_LISP note) so the bench "sbcl" column is really SBCL and
# not whatever the Roswell default happens to be (#35).
SBCL_RUN ?= ros -L sbcl-bin run
bench-survey: setup-cl-bench bench-build
	@echo "=== Survey dotcl (runs=$(RUNS) warmup=$(WARMUP)) ==="
	@EVAL_ARGS="--eval '(setq *bench-runs* $(RUNS))' --eval '(setq *bench-warmup* $(WARMUP))'"; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) $(BENCH_RUNTIME) --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-survey-dotcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; dotcl TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@echo "=== Survey SBCL (runs=$(RUNS) warmup=$(WARMUP)) ==="
	@EVAL_ARGS="--eval '(setq *bench-runs* $(RUNS))' --eval '(setq *bench-warmup* $(WARMUP))'"; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval timeout $(BENCH_TIMEOUT) $(SBCL_RUN) $$EVAL_ARGS --load $(DOTCL_ROOT)bench/run.lisp --eval "'(quit)'" 2>/tmp/bench-survey-sbcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; SBCL TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@# The aggregation runs on dotcl itself, so the public bench path needs no
	@# python3. -v:q keeps MSBuild's build output off stdout, which is the state
	@# file here; a build failure still stops the mv because the exit code is
	@# non-zero. (--nologo is not a `dotnet run` flag — it would be forwarded to
	@# the app and displace --asm from argv[0].)
	@$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -v:q -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)bench/make-survey-state.lisp /tmp/bench-survey-dotcl.txt /tmp/bench-survey-sbcl.txt $(DOTCL_ROOT)bench-state.json > /tmp/bench-state-new.json && mv /tmp/bench-state-new.json $(DOTCL_ROOT)bench-state.json
	@echo "Updated bench-state.json"

endif

setup-cl-bench:
	@if [ ! -d $(DOTCL_ROOT)cl-bench ]; then \
		echo "Cloning benkard/cl-bench..."; \
		git clone https://github.com/benkard/cl-bench.git $(DOTCL_ROOT)cl-bench; \
	else \
		echo "cl-bench/ already exists"; \
	fi

# cil-out.sil is the actual artifact; cross-compile is a phony alias kept
# for backward compatibility. Dependencies on $(CIL_SOURCES) and
# cil-compile.lisp let make skip rebuilds when no source has changed.
$(DOTCL_ROOT)compiler/cil-out.sil: $(CIL_SOURCES) $(DOTCL_ROOT)compiler/cil-compile.lisp
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$@" $(DOTCL_LISP) --load $(DOTCL_ROOT)compiler/cil-compile.lisp

cross-compile: $(DOTCL_ROOT)compiler/cil-out.sil

# --- Bootstrapping dotcl with dotcl -------------------------------------
#
# The compiler is ordinary ANSI Common Lisp, so any Common Lisp can host the
# cross-compile — including dotcl. Hosting it with dotcl takes the Lisp
# toolchain out of the build entirely: .NET is needed anyway, and a released
# dotcl installs as a .NET tool in ~4 s where installing Roswell in CI takes
# ~50 s. The per-build cost goes the other way (dotcl ~8 s vs SBCL ~4.6 s), so
# this is a CI fixed-cost trade, not a speedup.
#
# The seed is a RELEASED dotcl, never a locally built one: a build must not be
# able to poison the thing that builds it, and a pinned version is reproducible.
# It installs under build/ (gitignored, disposable) rather than the user's
# global tools, so `rm -rf build/seed` is always the way back.
#
# Not yet the default: 0.1.24 predates the fixes that let dotcl host its own
# cross-compile, and the core it produces from this tree does not work. The
# first release cut after those fixes can serve as the seed, and flipping the
# default is then this one line plus a CI lane swap.
DOTCL_SEED_VERSION ?= 0.1.25
DOTCL_SEED_DIR := $(DOTCL_ROOT)build/seed
DOTCL_SEED := $(DOTCL_SEED_DIR)/dotcl$(if $(filter Windows_NT,$(OS)),.cmd,)

$(DOTCL_SEED):
	@echo "=== Installing seed dotcl $(DOTCL_SEED_VERSION) into build/seed ==="
	dotnet tool install dotcl --version $(DOTCL_SEED_VERSION) --tool-path $(DOTCL_SEED_DIR)

seed-install: $(DOTCL_SEED)

# Can the pinned seed build this tree? Answers the one question that gates
# making dotcl the default host. Builds the core with the seed, then rebuilds it
# with the tree's own runtime (whose reader and codegen are newer), and requires
# the result to reproduce itself — the same generation check as selfhost-check.
seed-check: build $(DOTCL_SEED)
	@echo "=== Building the core with seed dotcl $(DOTCL_SEED_VERSION) ==="
	@mkdir -p $(DOTCL_ROOT)build/seed-check
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$(DOTCL_ROOT)build/seed-check/stage1.sil" \
	  $(DOTCL_SEED) --load $(DOTCL_ROOT)compiler/cil-compile.lisp
	@echo "=== Rebuilding it with the tree's runtime (stage 2) ==="
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$(DOTCL_ROOT)build/seed-check/stage2.sil" \
	  $(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- \
	  --asm $(DOTCL_ROOT)build/seed-check/stage1.sil --load $(DOTCL_ROOT)compiler/cil-compile.lisp
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$(DOTCL_ROOT)build/seed-check/stage3.sil" \
	  $(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- \
	  --asm $(DOTCL_ROOT)build/seed-check/stage2.sil --load $(DOTCL_ROOT)compiler/cil-compile.lisp
	@cmp $(DOTCL_ROOT)build/seed-check/stage2.sil $(DOTCL_ROOT)build/seed-check/stage3.sil \
	  && echo "seed $(DOTCL_SEED_VERSION): OK — stage2 == stage3" \
	  || (echo "seed $(DOTCL_SEED_VERSION): NOT usable — stage2 != stage3"; exit 1)

# The compiler compiled by itself must reproduce itself, byte for byte.
#
# A is the core the tree already has; B is what the compiler running out of A
# produces from the same sources; C is what B produces. B == C means the
# compiler is a fixpoint of itself. Comparing against the SBCL-hosted core would
# NOT show this: the two hosts number gensyms differently, so a byte comparison
# there is meaningless — it is the generation-to-generation comparison that has
# to hold.
#
# What this catches is a compiler that only works when SBCL is underneath it: a
# cross-compile flag that never reached the running compiler, and an intrinsic
# table keyed on the other half of a split package, both showed up here and
# nowhere else in the suite.
selfhost-check: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Self-host fixpoint (B == C) ==="
	@mkdir -p $(DOTCL_ROOT)build/selfhost
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$(DOTCL_ROOT)build/selfhost/genB.sil" \
	  $(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- \
	  --asm $(DOTCL_ROOT)compiler/cil-out.sil --load $(DOTCL_ROOT)compiler/cil-compile.lisp
	DOTCL_INPUTS="$(CIL_SOURCES)" DOTCL_OUTPUT="$(DOTCL_ROOT)build/selfhost/genC.sil" \
	  $(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- \
	  --asm $(DOTCL_ROOT)build/selfhost/genB.sil --load $(DOTCL_ROOT)compiler/cil-compile.lisp
	@cmp $(DOTCL_ROOT)build/selfhost/genB.sil $(DOTCL_ROOT)build/selfhost/genC.sil \
	  && echo "self-host fixpoint: OK (B == C)" \
	  || (echo "self-host fixpoint: FAILED — the compiler does not reproduce itself"; exit 1)

# The fixpoint says the compiler reproduces itself, not that what it produces is
# right — a compiler broken the same way twice is still a fixpoint. This runs the
# suite on the self-hosted core. Left out of CI deliberately: it re-runs the whole
# regression suite through a second core and roughly doubles the job.
selfhost-test: selfhost-check
	@echo "=== Regression suite on the self-hosted core ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- \
	  --asm $(DOTCL_ROOT)build/selfhost/genB.sil $(DOTCL_ROOT)test/regression/run.lisp

publish:
	dotnet publish $(DOTCL_ROOT)runtime/runtime.csproj --configuration Release -o $(DOTCL_ROOT)out/

# Compile contrib/asdf/asdf.lisp → asdf.fasl (.NET IL assembly) with dotcl
# itself. .fasl is the shipped artifact (fastest load); .sil and .lisp are
# not distributed. All 3 are gitignored.
$(DOTCL_ROOT)contrib/asdf/asdf.fasl: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)contrib/asdf/asdf.lisp $(RUNTIME_SOURCES)
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp")'

compile-asdf-fasl: setup-asdf $(DOTCL_ROOT)contrib/asdf/asdf.fasl

# Per-OS asdf fasls retired: a single OS-agnostic asdf.fasl is shipped.
# The .NET IL is portable and all OS-divergent behavior is resolved at run time
# (os-cond is runtime for dotcl), so target-features-per-OS baking is unnecessary.
compile-asdf-fasls: compile-asdf-fasl

# Compile contrib/quicklisp/quicklisp.lisp → quicklisp.fasl, same shape as asdf
# above: the .lisp is generated (concatenated client components) and the .fasl is
# the shipped artifact. Both are gitignored.
#
# asdf has to be loaded first: the client reads asdf: symbols at read time
# (client.lisp, dist.lisp, misc.lisp, setup.lisp), exactly as upstream's
# bootstrap loads asdf.lisp before the client files.
$(DOTCL_ROOT)contrib/quicklisp/quicklisp.fasl: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)contrib/quicklisp/quicklisp.lisp $(DOTCL_ROOT)contrib/asdf/asdf.fasl $(RUNTIME_SOURCES)
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(progn (require "asdf") (compile-file "$(DOTCL_ROOT)contrib/quicklisp/quicklisp.lisp"))'

compile-quicklisp-fasl: setup-quicklisp compile-asdf-fasl $(DOTCL_ROOT)contrib/quicklisp/quicklisp.fasl

# Pre-build IL fasls for every contrib that ships a .asd. Project-core
# builds consume these as ready artifacts instead of recompiling
# contrib source per project. Pattern rule matches contrib/<name>/<name>.lisp
# → contrib/<name>/<name>.fasl. asdf is handled separately above.
# CONTRIB_NAMES is auto-detected from contrib/*/ subdirs, so a tree that does
# not carry a given contrib skips it gracefully instead of failing on a missing
# directory (dotcl/dotcl#2).
CONTRIB_NAMES := $(filter-out asdf quicklisp cil-from-cs,$(notdir $(patsubst %/,%,$(wildcard $(DOTCL_ROOT)contrib/*/))))

CONTRIB_FASLS := $(foreach n,$(CONTRIB_NAMES),$(DOTCL_ROOT)contrib/$(n)/$(n).fasl)

# quicklisp is built by its own rule (it needs asdf loaded first) but is R2R'd
# like any other contrib: that step only crossgen2's an existing IL fasl.
CONTRIB_R2R_NAMES := $(CONTRIB_NAMES) quicklisp

# Order between contribs. A contrib whose source does (require "other") is
# compiled with that other contrib LOADED, and the loader prefers its prebuilt
# .fasl — so a stale one is what the compile sees. Nothing declares this
# relationship: the .asd files carry no :depends-on and the requirement is a
# (require ...) in the source, so it is read from the source here.
#
# Without it the build is alphabetical and breaks the day one contrib starts
# using a new export of another: advice (a) began calling dotnet:deref, which
# lives in dotnet-class (d), and compiling advice failed with "Symbol DEREF is
# not external in package DOTNET" against the older fasl.
CONTRIB_REQUIRE_NAMES = $(filter-out $(1),$(filter $(CONTRIB_NAMES),$(shell sed -n 's/.*(require "\([a-z0-9-]*\)").*/\1/p' $(DOTCL_ROOT)contrib/$(1)/$(1).lisp 2>/dev/null)))
define CONTRIB_ORDER_RULE
$(DOTCL_ROOT)contrib/$(1)/$(1).fasl: $(foreach d,$(call CONTRIB_REQUIRE_NAMES,$(1)),$(DOTCL_ROOT)contrib/$(d)/$(d).fasl)
endef
$(foreach n,$(CONTRIB_NAMES),$(eval $(call CONTRIB_ORDER_RULE,$(n))))

$(DOTCL_ROOT)contrib/%.fasl: $(DOTCL_ROOT)contrib/%.lisp $(DOTCL_ROOT)compiler/cil-out.sil $(RUNTIME_SOURCES)
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(compile-file "$<")'
	@n=$$(echo '$*' | cut -d/ -f1); \
	if [ -d "$(DOTCL_ROOT)runtime/contrib/$$n" ]; then \
	  cp "$@" "$(DOTCL_ROOT)runtime/contrib/$$n/$$n.fasl"; \
	fi

compile-contrib-fasls: $(CONTRIB_FASLS)

# Convert cil-out.sil → dotcl.core (PE assembly, FASL format) via
# dotcl:sil-to-fasl. The resulting .fasl loads in ~0.3s vs ~1.0s for .sil
# because Reader parse (~1.1s) + CIL assemble (~170ms) are both skipped.
# Ships in the pack as the default core.
$(DOTCL_ROOT)compiler/dotcl.core: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs $(RUNTIME_SOURCES)
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(dotcl:sil-to-fasl "$(DOTCL_ROOT)compiler/cil-out.sil" "$(DOTCL_ROOT)compiler/dotcl.core")'

compile-core-fasl: $(DOTCL_ROOT)compiler/dotcl.core

# R2R-compile dotcl.core / asdf.fasl per RID via crossgen2 cross-compile so
# each RID nupkg ships pre-native FASLs. Cold RunCore drops from ~3.37s to
# ~50ms, warm from ~107ms to ~16ms. crossgen2 host tool is
# whatever RID the build machine is on; --targetos / --targetarch produce
# code for any target.
R2R_RIDS := win-x64 win-arm64 linux-x64 linux-arm64 osx-x64 osx-arm64

# Map RID → (targetos, targetarch) for crossgen2 cross-compile flags.
TARGETOS_win-x64 := windows
TARGETARCH_win-x64 := x64
TARGETOS_win-arm64 := windows
TARGETARCH_win-arm64 := arm64
TARGETOS_linux-x64 := linux
TARGETARCH_linux-x64 := x64
TARGETOS_linux-arm64 := linux
TARGETARCH_linux-arm64 := arm64
TARGETOS_osx-x64 := osx
TARGETARCH_osx-x64 := x64
TARGETOS_osx-arm64 := osx
TARGETARCH_osx-arm64 := arm64

# crossgen2 host tool. RID is auto-detected from `dotnet --info`; binary
# name is `crossgen2.exe` on Windows, `crossgen2` elsewhere.
# .NET 8+ reports distro-specific RIDs (e.g. ubuntu.24.04-x64); normalize to
# portable form (linux-x64) that matches NuGet crossgen2 package names.
_DOTNET_RID := $(shell dotnet --info 2>/dev/null | awk '/^[[:space:]]*RID:/ {print $$2; exit}')
_HOST_ARCH  := $(lastword $(subst -, ,$(_DOTNET_RID)))
# $(strip ...) is REQUIRED: the multi-line $(if) below collapses each `\`
# continuation's indentation to a leading space, so without strip _HOST_OS
# becomes " linux" (leading space) on non-Windows hosts → HOST_RID " linux-x64"
# → the crossgen2 wildcard path gets a space and matches nothing
# ("crossgen2 not found (HOST_RID=  linux-x64)", #21). Windows takes the first
# (non-continued) branch so it was unaffected.
_HOST_OS    := $(strip $(if $(filter win-%,$(_DOTNET_RID)),win,\
               $(if $(findstring osx,$(_DOTNET_RID)),osx,\
               $(if $(findstring alpine,$(_DOTNET_RID)),linux-musl,linux))))
HOST_RID    := $(_HOST_OS)-$(_HOST_ARCH)
CROSSGEN2_EXE := $(if $(filter win-%,$(HOST_RID)),crossgen2.exe,crossgen2)

# gen-utils: C# codegen tool (download + char-names subcommands)
GEN_UTILS_EXE_NAME := $(if $(filter win-%,$(HOST_RID)),gen-utils.exe,gen-utils)
GEN_UTILS_OUT      := $(DOTCL_ROOT)scripts/gen-utils-out
GEN_UTILS_EXE      := $(GEN_UTILS_OUT)/$(GEN_UTILS_EXE_NAME)
GEN_UTILS_SRCS     := $(DOTCL_ROOT)scripts/GenUtils/Program.cs \
                      $(DOTCL_ROOT)scripts/GenUtils/GenUtils.csproj

$(GEN_UTILS_EXE): $(GEN_UTILS_SRCS)
	dotnet publish $(DOTCL_ROOT)scripts/GenUtils/GenUtils.csproj -o $(GEN_UTILS_OUT)/

$(DOTCL_ROOT)scripts/UnicodeData.txt: $(GEN_UTILS_EXE)
	$(GEN_UTILS_EXE) download https://unicode.org/Public/UCD/latest/ucd/UnicodeData.txt $@

$(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs: $(DOTCL_ROOT)scripts/UnicodeData.txt $(GEN_UTILS_EXE)
	$(GEN_UTILS_EXE) char-names $< $@

gen-char-names: $(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs
# NuGet package root, with forward slashes. On MSYS2 $(HOME) is a native Windows
# path (C:\Users\...) and a shell glob would eat the backslashes, so normalize
# here once; forward slashes are valid for both the shell and Windows itself.
# This is what lets the lookups below glob in the shell instead of $(wildcard).
NUGET_PKG_DIR := $(subst \,/,$(HOME))/.nuget/packages

# Pick the HIGHEST installed version, not the first match: a glob returns
# lexicographic order, so a cache holding both 10.0.x and a future 11.0.x would
# keep selecting the stale 10.0.x ("10" < "11" as strings) — hence sort -V.
# (Both build hosts — Linux CI and MSYS2 — ship GNU sort.)
#
# Glob with `ls`, NOT $(wildcard): make caches the directory listings it reads
# for a wildcard for the life of the process, so once a lookup finds no pack,
# every later $(wildcard) in the same make run keeps reporting none even after a
# publish restored it. That silently broke the per-RID R2R rules downstream of
# prime-crossgen2.
CROSSGEN2 = $(shell ls -d $(NUGET_PKG_DIR)/microsoft.netcore.app.crossgen2.$(HOST_RID)/*/tools/$(CROSSGEN2_EXE) 2>/dev/null | sort -V | tail -1)

# Cross-OS R2R needs the HOST crossgen2 pack, but a target-rid publish restores
# it only unreliably: the pack arrives as a side effect of PublishReadyToRun,
# which is non-deterministic when the host OS differs from the target OS (a
# cross-OS release CI would then die on "crossgen2 not found"). A HOST-rid
# publish is same-OS and restores the host crossgen2 pack deterministically, so
# prime it once before any per-RID R2R rule. Each R2R rule depends on this, so
# it runs first even under parallel make. Fails loudly if even the host publish
# cannot restore crossgen2 (a real environment problem, not a silent R2R skip).
.PHONY: prime-crossgen2
prime-crossgen2:
	@echo "=== prime host crossgen2 (HOST_RID=$(HOST_RID)) ==="
	dotnet publish $(DOTCL_ROOT)runtime/runtime.csproj -c Release -r $(HOST_RID) --self-contained false -p:PublishReadyToRun=true
	@# Locate the pack in the SHELL, not via $(CROSSGEN2). make expands EVERY line
	@# of a recipe before it runs the first one, so a make-level lookup here would
	@# report the state from before the publish above — this check could only ever
	@# pass when the pack happened to be cached already, and failed outright on a
	@# runner whose cache started empty. The publish output is left visible for the
	@# same reason: when the restore is the thing that went wrong, that log is the
	@# only evidence.
	@cg=$$(ls -d $(NUGET_PKG_DIR)/microsoft.netcore.app.crossgen2.$(HOST_RID)/*/tools/$(CROSSGEN2_EXE) 2>/dev/null | sort -V | tail -1); \
	if [ -z "$$cg" ]; then \
	  echo "error: host crossgen2 pack still missing after host publish (HOST_RID=$(HOST_RID))"; \
	  echo "crossgen2 packs under $(NUGET_PKG_DIR):"; \
	  ls -d $(NUGET_PKG_DIR)/microsoft.netcore.app.crossgen2.* 2>/dev/null || echo "  (none)"; \
	  exit 1; \
	fi; \
	echo "crossgen2: $$cg"

# Per-RID runtime ref dir (NuGet cache; populated by `dotnet publish -r <rid>`).
# Highest version, and shell glob rather than $(wildcard) — see CROSSGEN2 above
# for both. This one is restored by the publish inside the very rule that reads
# it, so the wildcard directory cache would bite here too.
runtime_ref = $(shell ls -d $(NUGET_PKG_DIR)/microsoft.netcore.app.runtime.$(1)/*/runtimes/$(1)/lib/net10.0 2>/dev/null | sort -V | tail -1)

# Generate compile-{core,asdf}-fasl-r2r-<rid> targets for each RID.
define R2R_RULES
compile-core-fasl-r2r-$(1): compile-core-fasl prime-crossgen2
	dotnet publish $$(DOTCL_ROOT)runtime/runtime.csproj -c Release -r $(1) --self-contained false -p:PublishReadyToRun=true >/dev/null
	@test -n "$$(CROSSGEN2)" || (echo "error: crossgen2 not found (HOST_RID=$(HOST_RID)). Is 'dotnet --info' showing the correct RID?" && exit 1)
	@test -n "$$(call runtime_ref,$(1))" || (echo "error: runtime ref for $(1) not found" && exit 1)
	cp $$(DOTCL_ROOT)compiler/dotcl.core $$(DOTCL_ROOT)compiler/dotcl.core.dll
	"$$(CROSSGEN2)" $$(DOTCL_ROOT)compiler/dotcl.core.dll \
	  -r "$$(call runtime_ref,$(1))/*.dll" \
	  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/runtime.dll" \
	  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/DotCL.Runtime.dll" \
	  --targetos $(TARGETOS_$(1)) --targetarch $(TARGETARCH_$(1)) -O \
	  -o $$(DOTCL_ROOT)compiler/dotcl-r2r-$(1).core
	rm -f $$(DOTCL_ROOT)compiler/dotcl.core.dll

compile-asdf-fasl-r2r-$(1): compile-asdf-fasl compile-core-fasl-r2r-$(1)
	@test -n "$$(CROSSGEN2)" || (echo "error: crossgen2 not found" && exit 1)
	cp $$(DOTCL_ROOT)contrib/asdf/asdf.fasl $$(DOTCL_ROOT)contrib/asdf/asdf.fasl.dll
	cp $$(DOTCL_ROOT)compiler/dotcl.core $$(DOTCL_ROOT)compiler/dotcl.core.dll
	"$$(CROSSGEN2)" $$(DOTCL_ROOT)contrib/asdf/asdf.fasl.dll \
	  -r "$$(call runtime_ref,$(1))/*.dll" \
	  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/runtime.dll" \
	  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/DotCL.Runtime.dll" \
	  -r "$$(DOTCL_ROOT)compiler/dotcl.core.dll" \
	  --targetos $(TARGETOS_$(1)) --targetarch $(TARGETARCH_$(1)) -O \
	  -o $$(DOTCL_ROOT)contrib/asdf/asdf-r2r-$(1).fasl
	rm -f $$(DOTCL_ROOT)contrib/asdf/asdf.fasl.dll $$(DOTCL_ROOT)compiler/dotcl.core.dll
endef

$(foreach rid,$(R2R_RIDS),$(eval $(call R2R_RULES,$(rid))))

compile-core-fasl-r2r-all: $(addprefix compile-core-fasl-r2r-,$(R2R_RIDS))
compile-asdf-fasl-r2r-all: $(addprefix compile-asdf-fasl-r2r-,$(R2R_RIDS))

# R2R-compile each contrib IL fasl per RID with the same crossgen2 pattern as
# asdf. Produces contrib/<name>/<name>-r2r-<rid>.fasl next to the IL fasl so the
# --target-rid dep resolver (DotclHost.ResolveDeps) probes and prefers it,
# falling back to the IL fasl when no R2R copy is present. Contrib fasls are
# compiled against runtime + core only (no cross-contrib assembly refs), so the
# reference set matches the asdf rule: per-RID runtime.dll + DotCL.Runtime.dll +
# dotcl.core (DotCL.Runtime holds the library body the fasls reference; without
# it crossgen2 resolves nothing and silently strips every method to non-R2R).
define R2R_CONTRIB_RULES
compile-contrib-fasls-r2r-$(1): compile-contrib-fasls compile-quicklisp-fasl compile-core-fasl-r2r-$(1)
	@test -n "$$(CROSSGEN2)" || (echo "error: crossgen2 not found" && exit 1)
	@test -n "$$(call runtime_ref,$(1))" || (echo "error: runtime ref for $(1) not found" && exit 1)
	cp $$(DOTCL_ROOT)compiler/dotcl.core $$(DOTCL_ROOT)compiler/dotcl.core.dll
	@for n in $$(CONTRIB_R2R_NAMES); do \
		fasl=$$(DOTCL_ROOT)contrib/$$$$n/$$$$n.fasl; \
		if [ ! -f "$$$$fasl" ]; then continue; fi; \
		echo "=== R2R contrib $$$$n ($(1)) ==="; \
		cp "$$$$fasl" "$$$$fasl.dll"; \
		"$$(CROSSGEN2)" "$$$$fasl.dll" \
		  -r "$$(call runtime_ref,$(1))/*.dll" \
		  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/runtime.dll" \
		  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/DotCL.Runtime.dll" \
		  -r "$$(DOTCL_ROOT)compiler/dotcl.core.dll" \
		  --targetos $(TARGETOS_$(1)) --targetarch $(TARGETARCH_$(1)) -O \
		  -o "$$(DOTCL_ROOT)contrib/$$$$n/$$$$n-r2r-$(1).fasl"; \
		rm -f "$$$$fasl.dll"; \
	done
	rm -f $$(DOTCL_ROOT)compiler/dotcl.core.dll
endef

$(foreach rid,$(R2R_RIDS),$(eval $(call R2R_CONTRIB_RULES,$(rid))))

compile-contrib-fasls-r2r-all: $(addprefix compile-contrib-fasls-r2r-,$(R2R_RIDS))

# Publish contrib/dotcl-cs helper DLL + Roslyn deps into
# contrib/dotcl-cs/lib/. Invoked during `make pack` so the tool NuGet
# bundles them under tools/net10.0/any/contrib/dotcl-cs/lib/.
# Users who never (require "dotcl-cs") never pay for loading these
# (~9MB of Roslyn).
contrib-dotcl-cs:
	rm -rf $(DOTCL_ROOT)contrib/dotcl-cs/lib $(DOTCL_ROOT)contrib/dotcl-cs/bin $(DOTCL_ROOT)contrib/dotcl-cs/obj
	dotnet publish $(DOTCL_ROOT)contrib/dotcl-cs/dotcl-cs.csproj -c Release -o $(DOTCL_ROOT)contrib/dotcl-cs/lib/ --self-contained false
	rm -f $(DOTCL_ROOT)contrib/dotcl-cs/lib/*.pdb $(DOTCL_ROOT)contrib/dotcl-cs/lib/*.deps.json
	rm -rf $(DOTCL_ROOT)contrib/dotcl-cs/bin $(DOTCL_ROOT)contrib/dotcl-cs/obj

# Build dotcl-jitdisasm contrib (dev tool; NOT included in make pack).
# After building, use: (require "dotcl-jitdisasm") then (dotcl:jit-disassemble #'fn)
contrib-dotcl-jitdisasm:
	rm -rf $(DOTCL_ROOT)contrib/dotcl-jitdisasm/lib $(DOTCL_ROOT)contrib/dotcl-jitdisasm/bin $(DOTCL_ROOT)contrib/dotcl-jitdisasm/obj
	dotnet publish $(DOTCL_ROOT)contrib/dotcl-jitdisasm/dotcl-jitdisasm.csproj -c Release -o $(DOTCL_ROOT)contrib/dotcl-jitdisasm/lib/ --self-contained false
	rm -f $(DOTCL_ROOT)contrib/dotcl-jitdisasm/lib/*.pdb $(DOTCL_ROOT)contrib/dotcl-jitdisasm/lib/*.deps.json
	rm -rf $(DOTCL_ROOT)contrib/dotcl-jitdisasm/bin $(DOTCL_ROOT)contrib/dotcl-jitdisasm/obj

# Extra args for the tool `dotnet pack` only. Release CI packs the full RID matrix
# and needs none of these. Locally, crossgen2 can only emit R2R for the host RID,
# and a RID it cannot compile suppresses the base pointer package for the whole
# pack — so a local package set has to be limited to the host:
#
#   make pack R2R_RIDS=win-arm64 PACK_ARGS=-p:RuntimeIdentifiers=win-arm64 \
#             PACK_VERSION=0.1.x-dev
#
# Keep R2R_RIDS and the RuntimeIdentifiers override in step: the recipe stages one
# dotcl-r2r-<rid>.core per R2R_RIDS entry. A ;-list cannot be passed here (see the
# note in runtime/runtime.csproj), so this only expresses a single RID. Such a set
# is what `dotcl pack --from` consumes.
PACK_ARGS ?=

# Version for the pack, applied to BOTH projects. Never pass -p:Version in
# PACK_ARGS: that reaches the tool pack only, and the library pack below runs
# afterwards and rewrites the shared bin/Release/net10.0/ output at the
# un-overridden version — leaving a runtime.dll that demands one DotCL.Runtime
# version next to a DotCL.Runtime.dll that claims another, which cannot start.
# The per-RID publish dirs (and so the nupkgs) stay consistent either way, so the
# breakage only shows up when running the plain build output.
PACK_VERSION ?=
_PACK_VERSION_ARG := $(if $(PACK_VERSION),-p:Version=$(PACK_VERSION),)

# Build NuGet package (requires cross-compile to have been run first).
# Nuke runtime/contrib first so a contrib directory deleted from source
# stops shipping in the nupkg (old dotcl-repl/ stayed in the
# installed tool for at least one release after its source was removed).
pack: compile-asdf-fasl compile-asdf-fasls compile-quicklisp-fasl compile-core-fasl compile-contrib-fasls contrib-dotcl-cs compile-core-fasl-r2r-all compile-asdf-fasl-r2r-all compile-contrib-fasls-r2r-all
	rm -rf $(DOTCL_ROOT)runtime/contrib
	cp $(DOTCL_ROOT)compiler/dotcl.core $(DOTCL_ROOT)runtime/dotcl.core
	@for rid in $(R2R_RIDS); do \
		cp $(DOTCL_ROOT)compiler/dotcl-r2r-$$rid.core $(DOTCL_ROOT)runtime/dotcl-r2r-$$rid.core; \
		cp $(DOTCL_ROOT)contrib/asdf/asdf-r2r-$$rid.fasl $(DOTCL_ROOT)runtime/asdf-r2r-$$rid.fasl; \
	done
	mkdir -p $(DOTCL_ROOT)runtime/contrib/asdf
	cp -r $(DOTCL_ROOT)contrib/*/ $(DOTCL_ROOT)runtime/contrib/
	rm -f $(DOTCL_ROOT)runtime/contrib/asdf/asdf.lisp $(DOTCL_ROOT)runtime/contrib/asdf/asdf.sil
	# Same for quicklisp: the concatenated client source is a build input, not a
	# shipped artifact, and it is 230 KB the module provider would never read
	# (it finds quicklisp.fasl first).
	rm -f $(DOTCL_ROOT)runtime/contrib/quicklisp/quicklisp.lisp
	# asdf's R2R copy is overlaid from runtime/ top level (ReplaceFaslsWithR2R
	# reads asdf-r2r-<rid>.fasl and writes it over contrib/asdf/asdf.fasl), so the
	# copies sitting under contrib/asdf/ are dead weight in every package. Every
	# other contrib keeps its per-RID R2R here and the csproj does the filtering:
	# the contrib/** glob drops all *-r2r-*.fasl and the RID being packed is added
	# back, so a package carries exactly one RID's set instead of all of them.
	rm -f $(DOTCL_ROOT)runtime/contrib/asdf/asdf-r2r-*.fasl
	rm -rf $(DOTCL_ROOT)runtime/contrib/dotcl-cs/bin $(DOTCL_ROOT)runtime/contrib/dotcl-cs/obj
	rm -f $(DOTCL_ROOT)runtime/contrib/dotcl-cs/*.csproj $(DOTCL_ROOT)runtime/contrib/dotcl-cs/*.cs
	cp $(DOTCL_ROOT)contrib/asdf/asdf.fasl $(DOTCL_ROOT)runtime/contrib/asdf/asdf.fasl
	dotnet pack $(DOTCL_ROOT)runtime/runtime.csproj --configuration Release -o $(DOTCL_ROOT)out/ $(PACK_ARGS) $(_PACK_VERSION_ARG)
	# Build the in-process project-core MSBuild task before packing the
	# library so DotCL.Runtime.csproj can bundle tasks/DotCL.Build.Tasks.dll.
	dotnet build $(DOTCL_ROOT)runtime/build-tasks/DotCL.Build.Tasks.csproj --configuration Release
	dotnet pack $(DOTCL_ROOT)runtime/DotCL.Runtime.csproj --configuration Release -o $(DOTCL_ROOT)out/ $(_PACK_VERSION_ARG)
	rm -f $(DOTCL_ROOT)runtime/dotcl.core
	@for rid in $(R2R_RIDS); do \
		rm -f $(DOTCL_ROOT)runtime/dotcl-r2r-$$rid.core $(DOTCL_ROOT)runtime/asdf-r2r-$$rid.fasl; \
	done

# Install as global dotnet tool from local package
install: pack
	dotnet tool uninstall -g dotcl 2>/dev/null || true
	dotnet tool install -g dotcl --add-source $(DOTCL_ROOT)out/

loc:
	@echo "=== Lisp (compiler) ==="
	@wc -l $(DOTCL_ROOT)compiler/*.lisp | sort -rn
	@echo ""
	@echo "=== C# (runtime) ==="
	@wc -l $(DOTCL_ROOT)runtime/*.cs | sort -rn
	@echo ""
	@echo "=== Test ==="
	@wc -l $(DOTCL_ROOT)test/ansi/*.lisp $(DOTCL_ROOT)test/*.lisp 2>/dev/null | tail -1
	@echo ""
	@echo "=== Total ==="
	@cat $(DOTCL_ROOT)compiler/*.lisp $(DOTCL_ROOT)runtime/*.cs $(DOTCL_ROOT)test/ansi/*.lisp $(DOTCL_ROOT)test/*.lisp 2>/dev/null | wc -l | xargs printf "  %s lines\n"

clean:
	rm -f $(DOTCL_ROOT)runtime/Generated.cs
	dotnet clean $(DOTCL_ROOT)runtime/runtime.csproj
