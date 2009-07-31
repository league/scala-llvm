package scala.tools.nsc
package interactive

import scala.collection._

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import dependencies._

import util.FakePos
import io.AbstractFile

/** A simple build manager, using the default scalac dependency tracker.
 *  The transitive closure of all dependent files on a modified file
 *  is recompiled at once.
 *
 *  It is equivalent to using a resident compiler mode with the
 *  '-make:transitive' option.
 */
class SimpleBuildManager(val settings: Settings) extends BuildManager {

  class BuilderGlobal(settings: Settings) extends scala.tools.nsc.Global(settings)  {
    def newRun() = new Run()
  }
  
  protected def newCompiler(settings: Settings) = new BuilderGlobal(settings) 

  val compiler = newCompiler(settings)

  /** Managed source files. */
  private val sources: mutable.Set[AbstractFile] = new mutable.HashSet[AbstractFile]

  /** Add the given source files to the managed build process. */
  def addSourceFiles(files: Set[AbstractFile]) {
    sources ++= files
    update(files)
  }

  /** Remove the given files from the managed build process. */
  def removeFiles(files: Set[AbstractFile]) {
    sources --= files
  }

  def update(added: Set[AbstractFile], removed: Set[AbstractFile]) {
    removeFiles(removed)
    update(added)
  }

  /** The given files have been modified by the user. Recompile
   *  them and all files that depend on them. Only files that
   *  have been previously added as source files are recompiled.
   */
  def update(files: Set[AbstractFile]) {
    val deps = compiler.dependencyAnalysis.dependencies
    val run = compiler.newRun()
    compiler.inform("compiling " + files)

    val toCompile = 
      (files ++ deps.dependentFiles(Int.MaxValue, files)) intersect sources
    

    compiler.inform("Recompiling " + 
                    (if(settings.debug.value) toCompile.mkString(", ")
                     else toCompile.size + " files"))
    
    buildingFiles(toCompile)

    run.compileFiles(files.toList)
  }

  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile) {
    compiler.dependencyAnalysis.loadFrom(file)
  }
  
  /** Save dependency information to `file'. */
  def saveTo(file: AbstractFile) {
    compiler.dependencyAnalysis.dependenciesFile = file
    compiler.dependencyAnalysis.saveDependencies()
  }
}
