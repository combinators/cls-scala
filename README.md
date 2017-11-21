# cls-scala
## The Combinatory Logic Synthesizer (CL)S Framework

This project implements the Combinatory Logic Synthesizer (CL)S framework in Scala.

TODO: More text & references here.

## Installation

You can create a new sbt project using:
```scala
  TODO: g8 template instruction here
```

Alternatively, you can add the following dependencies to your existing sbt project: 
```scala
libraryDependencies ++= 
  Seq("de.tu_dortmund.cs.ls14" %% "cls-scala" % "<VERSION>", // for the core algorithm
      "de.tu_dortmund.cs.ls14" %% "cls-scala-presentation" % "<VERSION>", // for a website presenting inhabitation results
      "de.tu_dortmund.cs.ls14" %% "cls-scala-templating-java" % "<VERSION>", // for java code templating support
      "de.tu_dortmund.cs.ls14" %% "cls-scala-templating-python" % "<VERSION>" // for python code templating support 
  )
```
In each line `<VERSION>` has to be replaced by the version you want.
You can search for released versions [here](http://search.maven.org/#search%7Cga%7C1%7Ccls-scala).

To obtain the latest unreleased development version, clone the repository and run `sbt publishLocal`.

Currently, Scala 2.11 and 2.12 are supported.

## Examples
Can be found in the [examples project](examples/src/main/scala/de/tu_dortmund/cs/ls14/cls) and the [tests](core/src/test/scala//src/main/scala/de/tu_dortmund/cs/ls14/cls).

## Help and Contributions

Join [combinators/cls-scala](https://gitter.im/combinators/cls-scala) on Gitter.

### Main Authors
- Jan Bessai
- Boris Düdder
- Geroge T. Heineman

### Contributers

### Your name here
Just the usual: open pull requests and or issues.
Feel free to add yourself to the list in this file, if you contribute something.
