DOTCL_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
DOTCL_LISP ?= ros run
INPUT ?= test/test1.lisp
STDBUF ?=
SETSID ?= $(shell which setsid 2>/dev/null)

.PHONY: all build run clean repl test-a2 test-ansi test-ansi-all test-ansi-full test-ansi-extra test-regression test-mop update-ansi-state commit-ansi-state cross-compile loc publish pack install setup-ansi-test setup-asdf setup-cl-bench bench bench-state test-sbcl-host2 compile-asdf-fasl compile-asdf-fasls compile-core-fasl compile-contrib-fasls contrib-dotcl-cs gen-char-names

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

test-ansi-extra: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running CLHS audit extra tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/test-ansi-extra.lisp

test-mop: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running AMOP protocol conformance tests ==="
	$(SETSID) dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test/mop-protocol.lisp

test-sbcl-host2: $(DOTCL_ROOT)compiler/cil-out.sil
	DOTNET_GCConserveMemory=7 dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)test-sbcl-host2.lisp

test-a2: build $(DOTCL_ROOT)compiler/cil-out.sil
	@echo "=== Running A2 (Lisp CIL compiler) tests ==="
	@for f in $(DOTCL_ROOT)test/test[0-9].lisp $(DOTCL_ROOT)test/test[12][0-9].lisp; do \
		[ -f "$$f" ] || continue; \
		echo -n "$$(basename $$f): "; \
		DOTCL_INPUTS="$$f" DOTCL_OUTPUT="/tmp/dotcl_instrs.sil" $(DOTCL_LISP) --load $(DOTCL_ROOT)compiler/cil-compile.lisp 2>/dev/null && \
		dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm /tmp/dotcl_instrs.sil 2>/dev/null \
			&& echo " OK" || echo " FAIL"; \
	done

test-ansi: build
	@echo "=== Running ANSI extracted tests ==="
	@if [ -d $(DOTCL_ROOT)test/ansi ]; then \
		for f in $(DOTCL_ROOT)test/ansi/*.lisp; do \
			echo -n "$$(basename $$f): "; \
			DOTCL_INPUTS="$(DOTCL_ROOT)test/framework.lisp $(DOTCL_ROOT)compiler/cil-stdlib.lisp $$f" \
				DOTCL_OUTPUT="/tmp/dotcl_instrs.sil" \
				$(DOTCL_LISP) --load $(DOTCL_ROOT)compiler/cil-compile.lisp 2>/dev/null && \
			dotnet run --project $(DOTCL_ROOT)runtime/runtime.csproj -- --asm /tmp/dotcl_instrs.sil 2>/dev/null \
				&& echo " OK" || echo " FAIL"; \
		done; \
	else \
		echo "No ansi test directory yet"; \
	fi

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
	@if [ ! -d $(DOTCL_ROOT)asdf ]; then \
		echo "Cloning asdf..."; \
		git clone https://github.com/dotcl/asdf.git $(DOTCL_ROOT)asdf; \
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
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; TIMEOUT after $(BENCH_TIMEOUT)s"; fi

# Generate bench-state.json with dotcl and SBCL results side by side
bench-state: setup-cl-bench
	@echo "=== Running benchmarks on dotcl ==="
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-dotcl.txt; \
	rc=$$?; if [ $$rc -eq 124 ]; then echo ";; dotcl TIMEOUT after $(BENCH_TIMEOUT)s"; fi
	@echo "=== Running benchmarks on SBCL ==="
	@EVAL_ARGS=""; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="--eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval timeout $(BENCH_TIMEOUT) ros run $$EVAL_ARGS --load $(DOTCL_ROOT)bench/run.lisp --eval "'(quit)'" 2>/tmp/bench-sbcl.txt; \
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
SBCL_RUN ?= ros run
bench-survey: setup-cl-bench
	@echo "=== Survey dotcl (runs=$(RUNS) warmup=$(WARMUP)) ==="
	@EVAL_ARGS="--eval '(setq *bench-runs* $(RUNS))' --eval '(setq *bench-warmup* $(WARMUP))'"; \
	if [ -n "$(SUITE)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-suite* :$(SUITE))'"; fi; \
	if [ -n "$(BENCH)" ]; then EVAL_ARGS="$$EVAL_ARGS --eval '(setq *bench-name* \"$(BENCH)\")'"; fi; \
	eval DOTNET_gcServer=0 $(SETSID) timeout $(BENCH_TIMEOUT) dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil $$EVAL_ARGS $(DOTCL_ROOT)bench/run.lisp 2>/tmp/bench-survey-dotcl.txt; \
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
# not distributed. All 3 are gitignored. D675 (was D673: .sil shipping).
$(DOTCL_ROOT)contrib/asdf/asdf.fasl: $(DOTCL_ROOT)compiler/cil-out.sil $(DOTCL_ROOT)contrib/asdf/asdf.lisp
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp")'

compile-asdf-fasl: setup-asdf $(DOTCL_ROOT)contrib/asdf/asdf.fasl

# Per-OS asdf fasls: compile asdf.lisp with target-features for each platform.
# These land in contrib/asdf/runtimes/{os}/asdf.fasl and are loaded by
# module-provide-contrib in preference to the generic asdf.fasl.
ASDF_TARGET_LINUX := (quote (:cl :common-lisp :dotcl :unix :linux :x86-64 :64-bit :little-endian))
ASDF_TARGET_WIN   := (quote (:cl :common-lisp :dotcl :windows :win32 :x86-64 :64-bit :little-endian))
ASDF_TARGET_OSX   := (quote (:cl :common-lisp :dotcl :unix :macos :darwin :bsd :x86-64 :64-bit :little-endian))

$(DOTCL_ROOT)contrib/asdf/runtimes/linux/asdf.fasl: setup-asdf $(DOTCL_ROOT)compiler/cil-out.sil
	mkdir -p $(DOTCL_ROOT)contrib/asdf/runtimes/linux
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil \
	  --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp" :output-file "$(DOTCL_ROOT)contrib/asdf/runtimes/linux/asdf.fasl" :target-features $(ASDF_TARGET_LINUX))'

$(DOTCL_ROOT)contrib/asdf/runtimes/win/asdf.fasl: setup-asdf $(DOTCL_ROOT)compiler/cil-out.sil
	mkdir -p $(DOTCL_ROOT)contrib/asdf/runtimes/win
	LOCALAPPDATA=/tmp/dotcl-cross-win APPDATA=/tmp/dotcl-cross-win \
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil \
	  --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp" :output-file "$(DOTCL_ROOT)contrib/asdf/runtimes/win/asdf.fasl" :target-features $(ASDF_TARGET_WIN))'

$(DOTCL_ROOT)contrib/asdf/runtimes/osx/asdf.fasl: setup-asdf $(DOTCL_ROOT)compiler/cil-out.sil
	mkdir -p $(DOTCL_ROOT)contrib/asdf/runtimes/osx
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil \
	  --eval '(compile-file "$(DOTCL_ROOT)contrib/asdf/asdf.lisp" :output-file "$(DOTCL_ROOT)contrib/asdf/runtimes/osx/asdf.fasl" :target-features $(ASDF_TARGET_OSX))'

compile-asdf-fasls: \
  $(DOTCL_ROOT)contrib/asdf/runtimes/linux/asdf.fasl \
  $(DOTCL_ROOT)contrib/asdf/runtimes/win/asdf.fasl \
  $(DOTCL_ROOT)contrib/asdf/runtimes/osx/asdf.fasl

# Pre-build IL fasls for every contrib that ships a .asd. Project-core
# builds (#166) consume these as ready artifacts instead of recompiling
# contrib source per project. Pattern rule matches contrib/<name>/<name>.lisp
# → contrib/<name>/<name>.fasl. asdf is handled separately above.
# CONTRIB_NAMES is auto-detected from contrib/*/ subdirs so that public
# mirror builds (where externally-sourced contribs are excluded via
# mirror-exclude) skip the missing dirs gracefully (dotcl/dotcl issue #2).
CONTRIB_NAMES := $(filter-out asdf,$(notdir $(patsubst %/,%,$(wildcard $(DOTCL_ROOT)contrib/*/))))

CONTRIB_FASLS := $(foreach n,$(CONTRIB_NAMES),$(DOTCL_ROOT)contrib/$(n)/$(n).fasl)

$(DOTCL_ROOT)contrib/%.fasl: $(DOTCL_ROOT)contrib/%.lisp $(DOTCL_ROOT)compiler/cil-out.sil
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(compile-file "$<")'
	@n=$$(echo '$*' | cut -d/ -f1); \
	if [ -d "$(DOTCL_ROOT)runtime/contrib/$$n" ]; then \
	  cp "$@" "$(DOTCL_ROOT)runtime/contrib/$$n/$$n.fasl"; \
	fi

compile-contrib-fasls: $(CONTRIB_FASLS)

# Convert cil-out.sil → dotcl.core (PE assembly, FASL format) via
# dotcl:sil-to-fasl. The resulting .fasl loads in ~0.3s vs ~1.0s for .sil
# because Reader parse (~1.1s) + CIL assemble (~170ms) are both skipped.
# Ships in the pack as the default core. (D677)
$(DOTCL_ROOT)compiler/dotcl.core: $(DOTCL_ROOT)compiler/cil-out.sil
	dotnet run --project $(DOTCL_ROOT)runtime -- --asm $(DOTCL_ROOT)compiler/cil-out.sil --eval '(dotcl:sil-to-fasl "$(DOTCL_ROOT)compiler/cil-out.sil" "$(DOTCL_ROOT)compiler/dotcl.core")'

compile-core-fasl: $(DOTCL_ROOT)compiler/dotcl.core

# R2R-compile dotcl.core / asdf.fasl per RID via crossgen2 cross-compile so
# each RID nupkg ships pre-native FASLs. Cold RunCore drops from ~3.37s to
# ~50ms, warm from ~107ms to ~16ms (D704/D705). crossgen2 host tool is
# whatever RID the build machine is on; --targetos / --targetarch produce
# code for any target. (D914 で win-arm64 から全 RID に拡張)
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
HOST_RID := $(shell dotnet --info 2>/dev/null | awk '/^[[:space:]]*RID:/ {print $$2; exit}')
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
CROSSGEN2 := $(firstword $(wildcard $(HOME)/.nuget/packages/microsoft.netcore.app.crossgen2.$(HOST_RID)/*/tools/$(CROSSGEN2_EXE)))

# Per-RID runtime ref dir (NuGet cache; populated by `dotnet publish -r <rid>`).
runtime_ref = $(firstword $(wildcard $(HOME)/.nuget/packages/microsoft.netcore.app.runtime.$(1)/*/runtimes/$(1)/lib/net10.0))

# Generate compile-{core,asdf}-fasl-r2r-<rid> targets for each RID.
define R2R_RULES
compile-core-fasl-r2r-$(1): compile-core-fasl
	@test -n "$$(CROSSGEN2)" || (echo "error: crossgen2 not found. Seed: 'dotnet publish -r $(1) -c Release /p:PublishReadyToRun=true'" && exit 1)
	@test -n "$$(call runtime_ref,$(1))" || (echo "error: runtime ref for $(1) not found. Seed: 'dotnet publish -r $(1) ...'" && exit 1)
	dotnet publish $$(DOTCL_ROOT)runtime/runtime.csproj -c Release -r $(1) --self-contained false -p:PublishReadyToRun=true >/dev/null
	cp $$(DOTCL_ROOT)compiler/dotcl.core $$(DOTCL_ROOT)compiler/dotcl.core.dll
	"$$(CROSSGEN2)" $$(DOTCL_ROOT)compiler/dotcl.core.dll \
	  -r "$$(call runtime_ref,$(1))/*.dll" \
	  -r "$$(DOTCL_ROOT)runtime/bin/Release/net10.0/$(1)/publish/runtime.dll" \
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
	  -r "$$(DOTCL_ROOT)compiler/dotcl.core.dll" \
	  --targetos $(TARGETOS_$(1)) --targetarch $(TARGETARCH_$(1)) -O \
	  -o $$(DOTCL_ROOT)contrib/asdf/asdf-r2r-$(1).fasl
	rm -f $$(DOTCL_ROOT)contrib/asdf/asdf.fasl.dll $$(DOTCL_ROOT)compiler/dotcl.core.dll
endef

$(foreach rid,$(R2R_RIDS),$(eval $(call R2R_RULES,$(rid))))

compile-core-fasl-r2r-all: $(addprefix compile-core-fasl-r2r-,$(R2R_RIDS))
compile-asdf-fasl-r2r-all: $(addprefix compile-asdf-fasl-r2r-,$(R2R_RIDS))

# Publish contrib/dotcl-cs helper DLL + Roslyn deps into
# contrib/dotcl-cs/lib/. Invoked during `make pack` so the tool NuGet
# bundles them under tools/net10.0/any/contrib/dotcl-cs/lib/.
# Users who never (require "dotcl-cs") never pay for loading these
# (~9MB of Roslyn). (D686, D903 で cil-from-cs / inline-cs 統合)
contrib-dotcl-cs:
	rm -rf $(DOTCL_ROOT)contrib/dotcl-cs/lib $(DOTCL_ROOT)contrib/dotcl-cs/bin $(DOTCL_ROOT)contrib/dotcl-cs/obj
	dotnet publish $(DOTCL_ROOT)contrib/dotcl-cs/dotcl-cs.csproj -c Release -o $(DOTCL_ROOT)contrib/dotcl-cs/lib/ --self-contained false
	rm -f $(DOTCL_ROOT)contrib/dotcl-cs/lib/*.pdb $(DOTCL_ROOT)contrib/dotcl-cs/lib/*.deps.json
	rm -rf $(DOTCL_ROOT)contrib/dotcl-cs/bin $(DOTCL_ROOT)contrib/dotcl-cs/obj

# Build NuGet package (requires cross-compile to have been run first).
# Nuke runtime/contrib first so a contrib directory deleted from source
# stops shipping in the nupkg (fixes D691: old dotcl-repl/ stayed in the
# installed tool for at least one release after its source was removed).
pack: compile-asdf-fasl compile-asdf-fasls compile-core-fasl compile-contrib-fasls contrib-dotcl-cs compile-core-fasl-r2r-all compile-asdf-fasl-r2r-all
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
	# duplicates them under tools/net10.0/<rid>/contrib/. (D922)
	rm -f $(DOTCL_ROOT)runtime/contrib/asdf/asdf-r2r-*.fasl
	rm -rf $(DOTCL_ROOT)runtime/contrib/dotcl-cs/bin $(DOTCL_ROOT)runtime/contrib/dotcl-cs/obj
	rm -f $(DOTCL_ROOT)runtime/contrib/dotcl-cs/*.csproj $(DOTCL_ROOT)runtime/contrib/dotcl-cs/*.cs
	cp $(DOTCL_ROOT)contrib/asdf/asdf.fasl $(DOTCL_ROOT)runtime/contrib/asdf/asdf.fasl
	dotnet pack $(DOTCL_ROOT)runtime/runtime.csproj --configuration Release -o $(DOTCL_ROOT)out/
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
