package org.combinators.cls

object TestCompat {
  type LazyList[+R] = scala.collection.immutable.Stream[R]
  private[cls] val LazyList = new StreamCompanionAdapter
  private[cls] class StreamCompanionAdapter {}
  implicit def toStreamCompanion(
    adapter: StreamCompanionAdapter
  ): scala.collection.immutable.Stream.type =
    scala.collection.immutable.Stream
}

