package de.tu_dortmund.cs.ls14.cls.persistable

import _root_.java.nio.file.{FileAlreadyExistsException, Files, Path}

/** Type class for persistable objects (inhabitants). */
trait Persistable {
  /** The type of the object to persist */
  type T
  /** Serialized String representation of the object */
  def rawText(elem: T): String
  /** Path where to store the object `elem` (relative to some later specified root) */
  def path(elem: T): Path

  /**
    * Computes the full path where to place `elem` relative to `basePath`.
    */
  def fullPath(basePath: Path, elem: T): Path = {
    basePath.resolve(path(elem))
  }


  /**
    * Persists this object to an object dependent path under `basePath`.
    * Overwrites any pre-existing files under `basePath` / `path`.
    */
  def persistOverwriting(basePath: Path, elem: T): Unit = {
    val fp = fullPath(basePath, elem)
    if (!Files.exists(fp.getParent))
      Files.createDirectories(fp.getParent)
    Files.write(fp, rawText(elem).getBytes)
  }

  /**
    * Persists this object to an object dependent path under `basePath`.
    * Throws an `FileAlreadyExistsException` if the file already exists.
    */
  def persist(basePath: Path, elem: T): Unit = {
    val fp = fullPath(basePath, elem)
    if (Files.exists(fp)) throw new FileAlreadyExistsException(fp.toString)
    else persistOverwriting(basePath, elem)
  }
}

/** Helper to obtain a type class instance for persistable objects. */
object Persistable {
  type Aux[TT] = Persistable { type T = TT }

  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}