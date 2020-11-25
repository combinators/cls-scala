# cls-scala
[![Maven Central](https://img.shields.io/maven-central/v/org.combinators/cls-scala_2.13.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.combinators%22%20AND%20%22cls-scala%22)
[![build status](https://github.com/combinators/cls-scala/workflows/Test%20code,%20update%20coverage,%20and%20release%20master%20branch/badge.svg?branch=master)](https://github.com/combinators/cls-scala/actions?query=workflow%3A%22Test+code%2C+update+coverage%2C+and+release+master+branch%22)
[![Coverage Status](https://coveralls.io/repos/github/combinators/cls-scala/badge.svg?branch=master)](https://coveralls.io/github/combinators/cls-scala?branch=master)
[![Join the chat at https://gitter.im/combinators/cls-scala](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/combinators/cls-scala)
## The Combinatory Logic Synthesizer (CL)S Framework

This project implements the Combinatory Logic Synthesizer (CL)S framework in Scala.

Existing users please refer to the [CHANGELOG](CHANGELOG.md) for news.

## Installation

Add the following dependency to your existing sbt project: 
```scala
libraryDependencies += "org.combinators" %% "cls-scala" % "<VERSION>"
```
The string `<VERSION>` has to be replaced by the version you want.
You can search for released versions [here](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.combinators%22%20AND%20a%3A%22cls-scala%22).

To obtain the latest unreleased development version, clone the repository and run `sbt publishLocal`.

Currently, Scala 2.11, 2.12, and 2.13 are supported.

## Examples

Can be found in the [examples project](examples/src/main/scala/org/combinators/cls/examples) and 
the [tests](src/test/scala/org/combinators/cls).

##### Got a new example?
If it is contained within one file, consider adding it to ours: 
join the [chat](https://gitter.im/combinators/cls-scala) or just open a pull request.

## Help and Contributions

Join [combinators/cls-scala](https://gitter.im/combinators/cls-scala) on Gitter.

### Main Authors

- Jan Bessai
- Boris Düdder
- George T. Heineman
- Anna Vasileva

### Contributers

-
##### Your name here?
Just the usual: open pull requests and or issues.
Feel free to add yourself to the list in this file, if you contributed something.
