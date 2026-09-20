# dotcl-thread

Threads with a bordeaux-threads compatible surface: `make-thread`,
`current-thread`, `thread-join`, `all-threads`, locks (`make-lock`,
`with-lock-held`, and the recursive variants), condition variables
(`condition-wait`, `condition-notify`, `condition-broadcast`) and semaphores.

    (require "dotcl-thread")

The threads are .NET threads. No dependencies.
