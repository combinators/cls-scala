package de.tu_dortmund.cs.ls14.cls.git

import java.nio.file._

import shapeless.feat.Enumeration
import de.tu_dortmund.cs.ls14.cls.inhabitation.Tree
import de.tu_dortmund.cs.ls14.cls.interpreter._
import de.tu_dortmund.cs.ls14.cls.persistable.Persistable
import de.tu_dortmund.cs.ls14.cls.types.Type
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.revwalk.RevCommit
import org.webjars.play.WebJarsUtil
import play.api.mvc._

/** Serves a website to access Git repositories with computed and serialized inhabitants.
  * To get the website, just inherit from this class, implement the abstract fields/methods
  * in `myresults.MyResults` and route like this:
  * <pre>
  * GET   &sol;myresults                        myresults.MyResults.overview()
  * GET   &sol;myresults&sol;raw_:number            myresults.MyResults.raw(number: Long)
  * GET   &sol;myresults&sol;prepare                myresults.MyResults.prepare(number: Long)
  * GET   &sol;myresults&sol;myresults.git&sol;*file    myresults.MyResults.serveFile(file)
  * </pre>
  *
  * If there are multiple inhabitants they go into branches of the git hostet at `/myresults/myresults.git`.
  *
  * @param webJars Helper to get WebJars like Bootstrap.
  */
abstract class InhabitationController(webJars: WebJarsUtil) extends InjectedController {
  /** A temporary place to store the Git repository on disk. */
  private lazy val root: Path = {
    val tmp = Files.createTempDirectory("inhabitants")
    tmp.toFile.deleteOnExit()
    tmp
  }
  /** The temporary Git file structure. */
  private lazy val git = Git.init().setDirectory(root.toFile).call()
  /** A mutable collection to store which inhabitants are already serialized. */
  private lazy val computedVariations = collection.mutable.Set.empty[Long]

  /** All combinator names together with their reflected type information. */
  val combinatorComponents: Map[String, CombinatorInfo]

  /** The path (relative to the Git root) where to store solutions. */
  val sourceDirectory: Path = Paths.get(".")

  /** A type class for heterogeneous vector of inhabitation Results */
  sealed trait InhabitationResultVector[R] {
    def add(newResults: R, oldResults: Results): Results
  }

  /** Type class instances to build up result vectors */
  sealed trait InhabitationResultVectorInstances {
    implicit def persistable[R](implicit persist: Persistable.Aux[R]): InhabitationResultVector[InhabitationResult[R]] =
      new InhabitationResultVector[InhabitationResult[R]] {
        def add(newResults: InhabitationResult[R], oldResults: Results): Results =
          oldResults.add[R](newResults)(persist)
      }

    implicit def product[L, R]
    (implicit persist: Persistable.Aux[R],
      vector: InhabitationResultVector[L]): InhabitationResultVector[(L, InhabitationResult[R])] =
      new InhabitationResultVector[(L, InhabitationResult[R])] {
        def add(newResults: (L, InhabitationResult[R]), oldResults: Results): Results =
          vector.add(newResults._1, oldResults).add[R](newResults._2)(persist)
      }
  }

  /** Collection of type class instances to build up result vectors */
  object InhabitationResultVector extends InhabitationResultVectorInstances {
    def apply[R](implicit vectorInst: InhabitationResultVector[R]): InhabitationResultVector[R] =
      vectorInst
  }

  /** A collection of persistable inhabitation results. */
  sealed trait Results {
    self =>
    /** Targets for this result collection. */
    val targets: Seq[(Type, Option[BigInt])]
    /** Raw inhabitant terms without any interpretation. */
    val raw: Enumeration[Seq[Tree]]
    /** Are there infinitely many inhabitants? */
    val infinite: Boolean
    /** Did any target not produce an inhabitant? */
    val incomplete: Boolean
    /** Actions to perform with each inhabitant (e.g. store to disk) */
    val persistenceActions: Enumeration[Seq[() => Unit]]

    /** Runs the action associated with the `number`-th inhabitant.
      * Most actions will just store the inhabitants to disk/Git (hence the method name).
      *
      * @param number index of the action to run.
      */
    def storeToDisk(number: Long): Unit = {
      val result = persistenceActions.index(number)
      result.foreach(_.apply())
    }


    /**
      * Adds an inhabitation result to the collection.
      * Create a default persistable using `inhabitationResult.toString` for serialization and `repositoryPath`.
      *
      * @param inhabitationResult the result to store.
      * @param repositoryPath     where to store `inhabitationResult` relative to the Git repository root.
      * @return a new collection including `inhabitationResult`.
      */
    def add[R](inhabitationResult: InhabitationResult[R], repositoryPath: Path): Results =
      add[R](inhabitationResult)(new Persistable {
        type T = R
        override def rawText(elem: T): String = elem.toString
        override def path(elem: T): Path = repositoryPath
      })

    /** Adds a [[Persistable]] result to the collection.
      * Persistance actions will be performed relative to the Git repository root.
      *
      * @param inhabitationResult the result to store.
      * @return a new collection including `inhabitationResult`.
      */
    def add[T](inhabitationResult: InhabitationResult[T])(implicit persistable: Persistable.Aux[T]): Results = {
      val size = inhabitationResult.size

      size match {
        case Some(x) if x == 0 => new Results {
          val targets = self.targets :+ (inhabitationResult.target, size)
          val raw = self.raw
          val persistenceActions = self.persistenceActions
          val infinite = self.infinite
          val incomplete = true
        }
        case _ => new Results {
          val targets = self.targets :+ (inhabitationResult.target, inhabitationResult.size)
          val raw = self.raw.product(inhabitationResult.terms).map {
            case (others, next) => others :+ next
          }
          val persistenceActions = self.persistenceActions.product(inhabitationResult.interpretedTerms).map {
            case (ps, r) => ps :+ (() => persistable.persist(root.resolve(sourceDirectory), r))
          }
          val infinite = self.infinite || inhabitationResult.isInfinite
          val incomplete = self.incomplete
        }
      }
    }

    /** Adds all results of an InhabitationResultVector. **/
    def addAll[R](results: R)(implicit canAddAll: InhabitationResultVector[R]): Results =
      canAddAll.add(results, this)
  }
  /** An empty collection of inhabitation results. */
  object Results extends Results {
    val targets: Seq[(Type, Option[BigInt])] = Seq.empty
    val raw: Enumeration[Seq[Tree]] = Enumeration.singleton(Seq.empty)
    val persistenceActions: Enumeration[Seq[() => Unit]] = Enumeration.singleton(Seq.empty)
    val infinite: Boolean = false
    val incomplete: Boolean = false
  }

  /** The results to present. */
  val results: Results

  /** Creates a new variation branch. */
  private def checkoutEmptyBranch(id: Long): Unit = {
    git
      .checkout()
      .setOrphan(true)
      .setName(s"variation_$id")
      .call()
    git.reset()
      .setMode(ResetType.HARD)
      .call()
  }

  /** Commits all files to the current branch */
  private def addAllFilesToCurrentBranch(): RevCommit = {
    git
      .add()
      .addFilepattern(".")
      .call()
    git
      .commit()
      .setMessage("Next variation")
      .call()
  }

  /** Creates the dumb protocol [[https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols]] file.
    *
    * @param rev branch head to create the file for.
    * @param id  variation number of the result.
    */
  private def updateInfo(rev: RevCommit, id: Long): Unit = {
    val info = Paths.get(root.toString, ".git", "info")
    Files.createDirectories(info)
    val refs = Paths.get(root.toString, ".git", "info", "refs")
    val line = s"${rev.getName}\trefs/heads/variation_$id\n"
    Files.write(refs, line.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
  }

  /** Creates a Git containing the `number`-th result.
    *
    * @param number index of the result to store.
    * @return term representation of the uninterpreted result.
    */
  def prepare(number: Long) = Action {
    val branchFile = Paths.get(root.toString, ".git", "refs", "heads", s"variation_$number")
    val result = results.raw.index(number).toString
    if (!Files.exists(branchFile)) {
      checkoutEmptyBranch(number)
      results.storeToDisk(number)
      val rev = addAllFilesToCurrentBranch()
      updateInfo(rev, number)
      computedVariations.add(number)
    }
    Ok(result)
  }

  /** Renders an overview page with access to all inhabitation results.
    *
    * @return the html code of the page.
    */
  def overview() = Action { request =>
    val combinators = combinatorComponents.mapValues {
      case staticInfo: StaticCombinatorInfo =>
        (ReflectedRepository.fullTypeOf(staticInfo),
          s"${scala.reflect.runtime.universe.show(staticInfo.fullSignature)}")
      case dynamicInfo: DynamicCombinatorInfo[_] =>
        (ReflectedRepository.fullTypeOf(dynamicInfo),
          dynamicInfo.position.mkString("\n"))
    }
    Ok(html.overview.render(
      request.path,
      webJars,
      combinators,
      results.targets,
      results.raw,
      computedVariations.toSet,
      results.infinite,
      results.incomplete))
  }

  /** Returns the uninterpreted raw representation of the `number`-th inhabitant. */
  def raw(number: Long) = Action {
    try {
      Ok(results.raw.index(number).toString())
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $number")
    }
  }

  /** Serves a file from the Git of all inhabitants.
    *
    * @param name file name relative to the Git root.
    * @return the file contents as an array of bytes.
    */
  def serveFile(name: String) = Action {
    try {
      Ok(Files.readAllBytes(root.resolve(Paths.get(".git", name))))
    } catch {
      case _: NoSuchFileException => play.api.mvc.Results.NotFound(s"404, File not found: $name")
      case _: AccessDeniedException => play.api.mvc.Results.Forbidden(s"403, Forbidden: $name")
    }
  }
}