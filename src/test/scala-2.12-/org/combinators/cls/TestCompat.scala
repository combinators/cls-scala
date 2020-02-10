package org.combinators.cls

object TestCompat {
  type LazyList = Stream
  val LazyList: Stream.type = LazyList
}

