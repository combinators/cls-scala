package org.combinators.cls

/** Compatability layer between Scala versions. */
package object compat {
  val ParallelCollectionConverters: scala.collection.parallel.CollectionConverters.type =
    scala.collection.parallel.CollectionConverters
}
