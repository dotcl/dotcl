# GC settings a running dotcl can change

dotcl runs on .NET, so a Lisp program shares the process-wide garbage collector
with its host and with every other .NET library in the process. This report lists
the GC settings that a running .NET 10 process can change, what they do, and what
they cost. It then measures the settings that could plausibly speed up an
allocation-heavy Lisp program, and recommends whether dotcl should add a scoped
form such as `(with-gc-latency (:batch) ...)`.

Measured on Linux x64 (6 logical CPUs, 5.8 GiB RAM) with .NET 10.0.402 and the
dotcl tree built by `make cross-compile`. The complete benchmark is
[`bench/gc-latency/gc-latency.lisp`](../bench/gc-latency/gc-latency.lisp).

## Settings

All of these are `System` or `System.Runtime` APIs. They are process-wide: they
are static properties and methods, and [the documentation says so explicitly for
`GCSettings.LatencyMode`][latency]. A Lisp thread cannot have a private GC
setting.

| Setting | What it does | GC flavours | Reversible | Docs |
| --- | --- | --- | --- | --- |
| `GCSettings.LatencyMode = Batch` | Disables concurrent collection and collects in batches. The most intrusive mode; maximum throughput at the expense of responsiveness. Overrides `gcConcurrent` and prevents further concurrent collections when background GC was on. | Workstation and server; background GC on or off. When background GC is off, Batch is already the default. | Yes, assign the previous value back. | [GCSettings.LatencyMode][latency-property], [GCLatencyMode][latency-enum], [Latency Modes][latency] |
| `GCSettings.LatencyMode = Interactive` | Enables concurrent collection; balances responsiveness and throughput. The default on a workstation with background GC on. | Workstation and server with background GC on. In a process started with `gcConcurrent=0`, assigning Interactive changes the reported property, while the start-up `ConcurrentGC` configuration stays false (observed on .NET 10). | Yes, assign the previous value back. | [GCLatencyMode][latency-enum], [Latency Modes][latency] |
| `GCSettings.LatencyMode = LowLatency` | Suppresses generation 2 collections; only generations 0 and 1 are collected. Intended for short critical sections. Not available for server GC. | Workstation only; background GC on or off. On server GC the assignment was silently ignored on .NET 10 (the previous mode stayed in effect). | Yes, assign the previous value back. | [GCLatencyMode][latency-enum], [Latency Modes][latency] |
| `GCSettings.LatencyMode = SustainedLowLatency` | Suppresses foreground generation 2 collections; generation 0, generation 1 and background generation 2 are still allowed. Intended for longer time-sensitive periods. The heap can grow, compaction is reduced, and fragmentation is more likely. Cannot be used when background GC is disabled; on .NET 10 the assignment was silently ignored in such a process. | Workstation and server; background GC must be on. | Yes, assign the previous value back. | [GCLatencyMode][latency-enum], [Latency Modes][latency] |
| `GC.TryStartNoGCRegion(totalSize[, lohSize[, disallowFullBlockingGC]])` / `GC.EndNoGCRegion()` | Tries to reserve a budget and suspend GC for a critical path. If the reservation succeeds, `GCSettings.LatencyMode` becomes `NoGCRegion`. The budget must cover application and runtime allocations. Calls cannot be nested. The region can end early if an allocation exceeds the budget or a GC is induced, and `EndNoGCRegion` then throws. | Workstation and server; background GC on or off. The reservation is process-wide, so allocating threads share the budget. | Only by `GC.EndNoGCRegion`, and only while the region is still active. The mode cannot be assigned through `LatencyMode`. | [GC.TryStartNoGCRegion][try-start], [GC.EndNoGCRegion][end-no-gc], [GCSettings.LatencyMode][latency-property] |
| `GCSettings.LargeObjectHeapCompactionMode = CompactOnce` | Makes the next full blocking generation 2 collection compact the large object heap too. The property then reverts to `Default`. It does not itself trigger a collection; a background generation 2 collection does not compact the LOH. | Workstation and server. | One-shot by design. Assign `Default` before the next blocking collection to cancel it. | [LargeObjectHeapCompactionMode][loh-property], [GCLargeObjectHeapCompactionMode][loh-enum] |
| `GC.RefreshMemoryLimit()` | Re-detects physical and container memory limits and re-reads heap hard-limit overrides. It is not a latency or throughput mode. It can throw if the new hard limit is below what is already committed or is otherwise invalid. | Workstation and server; 64-bit. The documented hard-limit settings are not handled on 32-bit systems. | Yes, in the sense that the data can be changed back and the method called again; this sets a new limit rather than restoring an old mode automatically. | [GC.RefreshMemoryLimit][refresh-limit] |
| `AppContext.SetData(name, value)` for GC keys | Stores a named value. It does not notify the GC. GC configuration is read when the GC is initialized, so setting `System.GC.Concurrent`, `System.GC.Server`, `System.GC.HeapCount` and similar keys after start-up has no effect. The exception is the heap hard-limit keys read by `GC.RefreshMemoryLimit` (`GCHeapHardLimit`, `GCHeapHardLimitPercent`, `GCHeapHardLimitSOH`, `GCHeapHardLimitLOH`, `GCHeapHardLimitPOH` and their percent forms), which can be changed and then applied with that call. | Not a GC flavour setting. The hard-limit keys apply to both workstation and server GC. | The stored data can be overwritten; it takes effect only on the next `GC.RefreshMemoryLimit`. | [AppContext.SetData][set-data], [GC config settings][gc-config], [GC.RefreshMemoryLimit][refresh-limit] |

Notes:

- `GCLatencyMode.NoGCRegion` is a read-only value. It is entered with
  `GC.TryStartNoGCRegion` and left with `GC.EndNoGCRegion`; assigning it to
  `GCSettings.LatencyMode` is invalid. [latency-property]
- `LowLatency` is documented as unavailable for server GC, and
  `SustainedLowLatency` is documented as unusable when background GC is disabled.
  On this .NET 10 build, the setter did not signal in either case; it left the
  previous mode unchanged. A caller that needs to know whether the mode took
  effect must read `GCSettings.LatencyMode` back.
- `GC.GetConfigurationVariables()` reports the effective configuration that the
  GC is using (for example `ConcurrentGC` and `ServerGC`). It is useful for
  confirming the start-up configuration, but it is not a way to change it.
  [get-config]

## Measurements

The benchmark has one timed section per process. The section first builds and
drops 60,000 lists of 1,000 conses, then inserts 180,000 pseudo-random keys into
a persistent unbalanced binary tree of structs and counts the resulting nodes.
`dotcl:gc-stats` reported about 2,504 MiB allocated per run, so the section
triggers many collections.

Each mode ran in a fresh child process, five times, interleaved round-robin with
the other modes so that slow drift on the VM affected every mode. Children were
forced to workstation GC (`DOTNET_gcServer=0`) and normal tiered JIT. Before the
mode was changed, each child warmed up with 35 repetitions of the list loop and
the tree build. For Batch and SustainedLowLatency, the mode was set immediately
before the timed section and restored in `unwind-protect`. For the whole-process
case, `DOTNET_gcConcurrent=0` was set in the child environment, so the mode was in
effect from process start. The default case used `DOTNET_gcConcurrent=1`.

Command, from the project root:

```sh
make cross-compile
dotnet run -c Release --project runtime/runtime.csproj -- \
  --asm compiler/cil-out.sil bench/gc-latency/gc-latency.lisp < /dev/null
```

The benchmark takes several minutes: it starts 20 dotcl processes and loads the
core in each one. Compare only rows from the same run of this command.

| Mode | Min (s) | Median (s) | Median / default | Mean allocated (MiB) | Mean GC pause (s) |
| --- | ---: | ---: | ---: | ---: | ---: |
| default | 2.161 | 2.214 | 1.000 | 2504.0 | 0.826 |
| `DOTNET_gcConcurrent=0`, whole process | 1.764 | 1.840 | 0.831 | 2504.0 | 0.390 |
| `LatencyMode = Batch`, timed section only | 2.156 | 2.189 | 0.989 | 2504.0 | 0.822 |
| `LatencyMode = SustainedLowLatency`, timed section only | 2.109 | 2.190 | 0.989 | 2504.0 | 0.820 |

The child configuration line printed by the benchmark confirmed that the
process-wide case really started with `ConcurrentGC=NIL`, while the other three
started with `ConcurrentGC=T`. It also confirmed that the scoped runs did set
`LatencyMode` to Batch and SustainedLowLatency before the timed section.

Interpretation:

- Starting with `DOTNET_gcConcurrent=0` is the only setting that helped. It
  reduced the median time by about 17% (1.20x faster) and roughly halved the
  reported blocking pause time. The direction agrees with the reported 2x
  effect, but this workload and VM gave 1.2x, not 2x.
- Scoped Batch and SustainedLowLatency were indistinguishable from the default.
  Batch is documented to prevent further concurrent collections, but switching
  it on for the timed section did not reproduce the gain of starting the process
  without background GC. The experiment does not separate the possible causes:
  the warm-up ran under background GC, the initial heap and background GC thread
  state differ, and the setting changes only collections after it is applied.
  Whatever the cause, a throughput-sensitive Lisp form cannot rely on scoped
  Batch to match `gcConcurrent=0`.
- SustainedLowLatency targets pauses, not throughput. Its larger heap and weaker
  compaction do not show up as a speedup in a short allocation-heavy run.
- `LowLatency`, `TryStartNoGCRegion`, LOH compaction and `RefreshMemoryLimit`
  were not benchmarked because they target pauses, LOH fragmentation or memory
  limits rather than this throughput question. Their documented scope and
  restrictions are in the table above.

## Recommendation

Do not add a scoped form such as `(with-gc-latency (:batch) ...)` as a
throughput optimization. It would not deliver the effect that
`DOTNET_gcConcurrent=0` delivers for allocation-heavy Lisp, and `:batch` is the
wrong mode to wrap around ordinary Lisp work.

A scoped low-latency form can still be useful to an application that has a real
latency-critical section, but it should be an explicit opt-in API, not an
automatic dotcl optimization. If dotcl adds one, `:sustained-low-latency` is the
mode with the widest applicability: it works on workstation and server GC and is
intended for longer sections than `LowLatency`. Such a form must document these
side effects:

- The setting is process-wide. It changes collection behavior for the host's
  threads and every other .NET library while the body runs, not just for the
  Lisp thread. The documentation warns that because the setting is process-wide,
  an `OutOfMemoryException` can be generated on any thread that is allocating
  during a low-latency period. [latency]
- Low-latency modes make the GC more conservative. They can grow the heap, avoid
  compaction and increase fragmentation. That is the intended trade-off for
  pause-sensitive work, and it is the wrong trade-off for a batch job.
- SustainedLowLatency is ignored when background GC is disabled, and LowLatency
  is unavailable on server GC. The form should either signal or return a clear
  "not applied" result after reading the mode back, rather than silently doing
  nothing.
- Restoring the old value in `unwind-protect` handles a non-local exit from a
  single owner, but the setting is global. Two threads that each save and restore
  it can restore stale values, and code outside the form can change it at any
  time. A safe API would either take a process-wide lock and document that no
  other code may change the mode, or expose the mode as a plain setter/getter and
  leave the policy to the application.
- `TryStartNoGCRegion` is a poor fit for a general Lisp form. The caller must
  guess a byte budget, the region is not nestable, any thread can exhaust the
  budget, and the region may already have ended before the form tries to close
  it. A wrapper would have to return success or failure, not just run a body.

For the specific problem in the background, the host should choose
`System.GC.Concurrent=false` in its runtime configuration or
`DOTNET_gcConcurrent=0` before the process starts, as a process-wide decision
that includes all embedded libraries. dotcl must not change that default itself.

## Sources

- [GCSettings.LatencyMode property][latency-property]
- [GCLatencyMode enumeration][latency-enum]
- [Latency Modes][latency]
- [GC.TryStartNoGCRegion][try-start]
- [GC.EndNoGCRegion][end-no-gc]
- [GCSettings.LargeObjectHeapCompactionMode][loh-property]
- [GCLargeObjectHeapCompactionMode enumeration][loh-enum]
- [GC.RefreshMemoryLimit][refresh-limit]
- [AppContext.SetData][set-data]
- [Garbage collector config settings][gc-config]
- [GC.GetConfigurationVariables][get-config]
- [Workstation vs. server garbage collection][ws-server]

[latency-property]: https://learn.microsoft.com/en-us/dotnet/api/system.runtime.gcsettings.latencymode?view=net-10.0
[latency-enum]: https://learn.microsoft.com/en-us/dotnet/api/system.runtime.gclatencymode?view=net-10.0
[latency]: https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/latency
[try-start]: https://learn.microsoft.com/en-us/dotnet/api/system.gc.trystartnogcregion?view=net-10.0
[end-no-gc]: https://learn.microsoft.com/en-us/dotnet/api/system.gc.endnogcregion?view=net-10.0
[loh-property]: https://learn.microsoft.com/en-us/dotnet/api/system.runtime.gcsettings.largeobjectheapcompactionmode?view=net-10.0
[loh-enum]: https://learn.microsoft.com/en-us/dotnet/api/system.runtime.gclargeobjectheapcompactionmode?view=net-10.0
[refresh-limit]: https://learn.microsoft.com/en-us/dotnet/api/system.gc.refreshmemorylimit?view=net-10.0
[set-data]: https://learn.microsoft.com/en-us/dotnet/api/system.appcontext.setdata?view=net-10.0
[gc-config]: https://learn.microsoft.com/en-us/dotnet/core/runtime-config/garbage-collector
[get-config]: https://learn.microsoft.com/en-us/dotnet/api/system.gc.getconfigurationvariables?view=net-10.0
[ws-server]: https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/workstation-server-gc
