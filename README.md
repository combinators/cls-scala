# cls-scala
## The Combinatory Logic Synthesizer (CL)S Framework

This project implements the Combinatory Logic Synthesizer (CL)S framework in Scala.

TODO: More text & references here.

## Installation
To obtain the latest version, clone the reopsitory and run sbt and publishLocal.

The current release is available at maven central, just add 
```scala
libraryDependencies ++= 
  Seq("de.tu_dortmund.cs.ls14" %% "cls-scala" % "<LATEST VERSION>", // for the core algorithm
      "de.tu_dortmund.cs.ls14" %% "cls-scala-presentation" % "<LATEST VERSION>", // for a website presenting inhabitation results
      "de.tu_dortmund.cs.ls14" %% "cls-scala-templating-java" % "<LATEST VERSION>", // for java code templating support
      "de.tu_dortmund.cs.ls14" %% "cls-scala-templating-python" % "<LATEST VERSION>" // for python code templating support 
  )
```
You can create a default project with the correct settings using
```scala
  TODO: g8 template instruction here
```

Currently, Scala 2.11 and 2.12 are supported in the released version.

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
