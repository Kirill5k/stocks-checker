# Stocks Checker

A Scala service for collecting and analyzing stock market data.

## Building and running

Install Java 27 and an sbt launcher. Set `JAVA_HOME` to your Java 27 installation
and ensure `java` is available on `PATH`. The project uses Scala 3.10.0 and pins
sbt 2.0.11 in `project/build.properties`.

```sh
sbt about
sbt compile
sbt test
sbt scalafmtCheck
sbt "core/run"
```

Running the service requires API keys and MongoDB configuration; see
[AGENTS.md](AGENTS.md#configuration) for environment variables.

## sbt exits without output

A zero-byte `project/target/active.json` can make the sbt client exit silently.
To check whether the build itself loads, bypass the client with:

```sh
sbt --server about
```

Close this project's sbt sessions and IDE build sessions, then remove the server
discovery file only if it exists and is empty:

```sh
if [ -f project/target/active.json ] && [ ! -s project/target/active.json ]; then
  rm project/target/active.json
fi
sbt about
```

sbt recreates its discovery file as needed. No permanent `--server` configuration
is required.
