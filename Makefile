DOTCL_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
# Cross-compile host. Pin SBCL via Roswell so the build doesn't silently use
# whatever `ros` default the user happens to have (installing another impl, e.g.
# ABCL, flips the default and breaks cross-compile, dotcl/dotcl #35). Still
# overridable, e.g. `DOTCL_LISP=dotcl make cross-compile` to self-host.
DOTCL_LISP ?= ros -L sbcl-bin run
STDBUF ?=
SETSID ?= $(shell which setsid 2>/dev/null)

.PHONY: all build build-ns2 run clean repl test-ansi-all test-ansi-full test-ansi-extra test-regression test-mop ilverify update-ansi-state commit-ansi-state cross-compile loc publish pack install setup-ansi-test setup-asdf setup-cl-bench bench bench-state test-sbcl-host2 compile-asdf-fasl compile-asdf-fasls compile-core-fasl compile-contrib-fasls contrib-dotcl-cs contrib-dotcl-jitdisasm gen-char-names

# Source files for cross-compile. Listed once; the recipe and dependency
# tracking both reference this so adding a file is a single-edit change.
CIL_SOURCES := \
  $(DOTCL_ROOT)compiler/cil-compiler.lisp \
  $(DOTCL_ROOT)compiler/cil-stdlib.lisp \
  $(DOTCL_ROOT)compiler/cil-macros.lisp \
  $(DOTCL_ROOT)compiler/loop.lisp \
  $(DOTCL_ROOT)compiler/cil-analysis.lisp \
  $(DOTCL_ROOT)compiler/cil-forms.lisp

all: cross-compile build

build: $(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs
	dotnet build $(DOTCL_ROOT)runtime/runtime.csproj

run:
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj

repl:
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --repl

test-regression: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running dotcl regression tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/regression/run.lisp

test-debug-pdb: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running debug-path (DOTCL_EMIT_PDB) checks ==="
	sh $(DOTCL_ROOT)test/debug-pdb/check.sh $(DOTCL_ROOT)

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

test-sbcl-host2: $(DOTCL_ROOT)compiler/cil-out.sil
	DOTNET_GCConserveMemory=7 dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test-sbcl-host2.lisp

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
		{ \
		echo '{'; \
		echo '  "updated": "'"$$(date +%Y-%m-%d)"'",'; \
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

setup-asdf:
	@# dotcl-0.1.11 is the compat-generation bundle branch: it pairs with the
	@# launch-process keyword API and the run-time os-cond / single-FASL
	@# work. Updated in place going forward; a new dotcl-X.Y.Z branch is cut
	@# only on the next hard #+dotcl incompatibility. The old `dotcl` branch stays
	@# frozen so pre-0.1.11 source builds keep cloning a matching asdf.
	@if [ ! -d $(DOTCL_ROOT)asdf ]; then \
		echo "Cloning asdf..."; \
		git clone --branch dotcl-0.1.11 https://github.com/dotcl/asdf.git $(DOTCL_ROOT)asdf; \
	else \
		echo "asdf/ already exists"; \
	fi
	@if [ ! -f $(DOTCL_ROOT)asdf/build/asdf.lisp ]; then \
		echo "Building asdf..."; \
		cd $(DOTCL_ROOT)asdf && sh make-asdf.sh; \
	fi
	@mkdir -p $(DOTCL_ROOT)contrib/asdf
	@# cmp-then-cp so unchanged source doesn't bump dest mtime (which would
	@# cascade-rebuild asdf.fasl unnecessarily on every compile-asdf-fasl call).
	@cmp -s $(DOTCL_ROOT)asdf/build/asdf.lisp $(DOTCL_ROOT)contrib/asdf/asdf.lisp 2>/dev/null \
	  || cp $(DOTCL_ROOT)asdf/build/asdf.lisp $(DOTCL_ROOT)contrib/asdf/asdf.lisp

# Benchmarks: make bench / make bench SUITE=gabriel / make bench BENCH=tak
SUITE ?=
BENCH ?=
BENCH_TIMEOUT ?= 600
bench: setup-cl-bench
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; TIMEOUT after $(BENCH_TIMEOUT)s"; fi

# Generate bench-state.json with dotcl and SBCL results side by side
bench-state: setup-cl-bench
	@echo "=== Running benchmarks on dotcl ==="
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-dotcl.txt; \
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
PYTHON ?= python3
# Pin SBCL (see DOTCL_LISP note) so the bench "sbcl" column is really SBCL and
# not whatever the Roswell default happens to be (#35).
SBCL_RUN ?= ros -L sbcl-bin run
bench-survey: setup-cl-bench
	@echo "=== Survey dotcl (runs=$(RUNS) warmup=$(WARMUP)) ==="
	@EVAL_ARGS="--eval '(setq *bench-runs* $(RUNS))' --eval '(setq *bench-warmup* $(WARMUP))'"; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-survey-dotcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; dotcl TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@echo "=== Survey SBCL (runs=$(RUNS) warmup=$(WARMUP)) ==="
	@EVAL_ARGS="--eval '(setq *bench-runs* $(RUNS))' --eval '(setq *bench-warmup* $(WARMUP))'"; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval timeout $(BENCH_TIMEOUT) $(SBCL_RUN) $$EVAL_ARGS --load $(DOTCL_ROOT)bench/run.lisp --eval "'(quit)'" 2>/tmp/bench-survey-sbcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; SBCL TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@$(PYTHON) $(DOTCL_ROOT)bench/make-survey-state.py /tmp/bench-survey-dotcl.txt /tmp/bench-survey-sbcl.txt $(DOTCL_ROOT)bench-state.json > /tmp/bench-state-new.json && mv /tmp/bench-state-new.json $(DOTCL_ROOT)bench-state.json
	@echo "Updated bench-state.json"

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

publish:
	dotnet publish $(DOTCL_ROOT)runtime/runtime.csproj --configuration Release -o $(DOTCL_ROOT)out/

# Compile contrib/asdf/asdf.lisp → asdf.fasl (.NET IL assembly) with dotcl
# itself. .fasl is the shipped artifact (fastest load); .sil and .lisp are
# not distributed. All 3 are gitignored.
$(DOTCL_ROOT)contrib/asdf/asdf.fasl: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)contrib/asdf/asdf.lisp
	dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp")'

compile-asdf-fasl: setup-asdf $(DOTCL_ROOT)contrib/asdf/asdf.fasl

# Per-OS asdf fasls retired: a single OS-agnostic asdf.fasl is shipped.
# The .NET IL is portable and all OS-divergent behavior is resolved at run time
# (os-cond is runtime for dotcl), so target-features-per-OS baking is unnecessary.
compile-asdf-fasls: compile-asdf-fasl

# Pre-build IL fasls for every contrib that ships a .asd. Project-core
# builds consume these as ready artifacts instead of recompiling
# contrib source per project. Pattern rule matches contrib/<name>/<name>.lisp
# → contrib/<name>/<name>.fasl. asdf is handled separately above.
# CONTRIB_NAMES is auto-detected from contrib/*/ subdirs so that public
# mirror builds (where externally-sourced contribs are excluded via
# mirror-exclude) skip the missing dirs gracefully (dotcl/dotcl issue #2).
CONTRIB_NAMES := $(filter-out asdf cil-from-cs,$(notdir $(patsubst %/,%,$(wildcard $(DOTCL_ROOT)contrib/*/))))

CONTRIB_FASLS := $(foreach n,$(CONTRIB_NAMES),$(DOTCL_ROOT)contrib/$(n)/$(n).fasl)

$(DOTCL_ROOT)contrib/%.fasl: $(DOTCL_ROOT)contrib/%.lisp $(DOTCL_ROOT)compiler/cil-out.sil
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
$(DOTCL_ROOT)compiler/dotcl.core: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)runtime/Generated/UnicodeCharNames.g.cs
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
# Pick the HIGHEST installed version, not $(firstword $(wildcard ...)): wildcard
# returns lexicographic order, so a cache holding both 10.0.x and a future 11.0.x
# would keep selecting the stale 10.0.x ("10" < "11" as strings). $(wildcard)
# still does the globbing (it handles the native $(HOME) path); we only sort its
# result by version. (Both build hosts — Linux CI and MSYS2 — ship GNU sort.)
CROSSGEN2 = $(shell echo "$(wildcard $(HOME)/.nuget/packages/microsoft.netcore.app.crossgen2.$(HOST_RID)/*/tools/$(CROSSGEN2_EXE))" | tr ' ' '\n' | sort -V | tail -1)

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
	dotnet publish $(DOTCL_ROOT)runtime/runtime.csproj -c Release -r $(HOST_RID) --self-contained false -p:PublishReadyToRun=true >/dev/null
	@test -n "$(CROSSGEN2)" || (echo "error: host crossgen2 pack still missing after host publish (HOST_RID=$(HOST_RID))" && exit 1)
	@echo "crossgen2: $(CROSSGEN2)"

# Per-RID runtime ref dir (NuGet cache; populated by `dotnet publish -r <rid>`).
# Highest version, not lexicographic firstword — see CROSSGEN2 above.
runtime_ref = $(shell echo "$(wildcard $(HOME)/.nuget/packages/microsoft.netcore.app.runtime.$(1)/*/runtimes/$(1)/lib/net10.0)" | tr ' ' '\n' | sort -V | tail -1)

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
compile-contrib-fasls-r2r-$(1): compile-contrib-fasls compile-core-fasl-r2r-$(1)
	@test -n "$$(CROSSGEN2)" || (echo "error: crossgen2 not found" && exit 1)
	@test -n "$$(call runtime_ref,$(1))" || (echo "error: runtime ref for $(1) not found" && exit 1)
	cp $$(DOTCL_ROOT)compiler/dotcl.core $$(DOTCL_ROOT)compiler/dotcl.core.dll
	@for n in $$(CONTRIB_NAMES); do \
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
pack: compile-asdf-fasl compile-asdf-fasls compile-core-fasl compile-contrib-fasls contrib-dotcl-cs compile-core-fasl-r2r-all compile-asdf-fasl-r2r-all compile-contrib-fasls-r2r-all
	rm -rf $(DOTCL_ROOT)runtime/contrib
	cp $(DOTCL_ROOT)compiler/dotcl.core $(DOTCL_ROOT)runtime/dotcl.core
	@for rid in $(R2R_RIDS); do \
		cp $(DOTCL_ROOT)compiler/dotcl-r2r-$$rid.core $(DOTCL_ROOT)runtime/dotcl-r2r-$$rid.core; \
		cp $(DOTCL_ROOT)contrib/asdf/asdf-r2r-$$rid.fasl $(DOTCL_ROOT)runtime/asdf-r2r-$$rid.fasl; \
	done
	mkdir -p $(DOTCL_ROOT)runtime/contrib/asdf
	cp -r $(DOTCL_ROOT)contrib/*/ $(DOTCL_ROOT)runtime/contrib/
	rm -f $(DOTCL_ROOT)runtime/contrib/asdf/asdf.lisp $(DOTCL_ROOT)runtime/contrib/asdf/asdf.sil
	# Strip cross-RID R2R fasls from contrib/asdf/ so each RID nupkg only ships
	# its own R2R copy (overlaid by ReplaceFaslsWithR2R via runtime/asdf-r2r-<rid>.fasl
	# which is at runtime/ top-level, separate from contrib/). Without this the
	# `<None Include="contrib/**" PackagePath="tools/net10.0/any/contrib/">` glob
	# packs all 6 R2R fasls into every RID's nupkg, and dotnet publish further
	# duplicates them under tools/net10.0/<rid>/contrib/.
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
