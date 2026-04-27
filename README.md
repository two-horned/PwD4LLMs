# Parsing with Derivatives for Improving LLM Code Generation

## Overview

This sbt project is seperated into three.
1) The library, consisting essential trait definitions;
2) An example implementation, defining a parser
   and token generator for the PCF language.
3) A benchmark. Ready-to-run JMH Benchmark measuring
   The performance of various token generation strategies.

## Dependencies

* Scala version `3.7.4`
* sbt version `1.12.6`
* first-class-derivatives version `3.0.1`

For installing Scala and sbt, refer to their appropriate installation guide online.

https://docs.scala-lang.org/getting-started/install-scala.html

https://www.scala-sbt.org/1.x/docs/Setup.html


To install first-class-derivatives, create a local copy of [my repository](github.com/two-horned/fcd).
```sh
git clone github.com/two-horned/fcd --depth=1
```

Then publish the artifact library locally.
```sh
cd fcd/artifact
sbt publishLocal
```

The library, example implementation and benchmark setup are
accessible by using `library`, `example` or `bench` keyword
respectively for the sbt command of choice.

For example, accessing the library by itself in a console:
```sh
sbt library/run
```

For example, running the example:
```sh
sbt example/run
```

Or starting the benchmark:
```sh
sbt bench/Jmh/run -- -f 5 -t 5 -rf json -rff results.json
```
