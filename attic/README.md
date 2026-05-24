# attic

Scripts and configuration that the project no longer ships, kept here
for sentimental and historical value rather than active use.

## `bin/thread_watch.sh`, `bin/enable_raw_mode.sh`, `bin/cmdarg.sh`

Carried over from the original `thread-watch` tool that this project
forked off of. `thread_watch.sh` ran `jstack` in a `watch` loop and
piped each new dump through a thread-watch jar to monitor one specific
thread's stack trace over time. `enable_raw_mode.sh` was a wrapper
around an interactive terminal mode, and `cmdarg.sh` is the vendored
[cmdarg](https://github.com/akesterson/cmdarg) bash arg-parser the
shell scripts depended on. None of them target jstack-report.

## `bin/create-babashka-script.sh`

Concatenated `src/jstack_report/ansi.clj` and `src/jstack_report/core.clj`
into a single self-executing babashka script. Worked while the project
lived in two files; the namespace split made it obsolete. Worth
revisiting if a single-file babashka distribution becomes interesting.

## `script/compile`

GraalVM `native-image` build script. Was never wired into CI, references
the old `jstack-report-0.1.0` jar name, and assumes a `GRAALVM_HOME`
pointing at a JDK with `native-image` installed. Kept for whoever wants
to revive the native binary route.

## `project.clj`

The Leiningen project file from before the switch to deps.edn +
tools.build (v1.3.0). Useful as a reference if you want to add a lein
target back, or to remind yourself how the `:reflection` /
`lein-git-version` plumbing was configured.
