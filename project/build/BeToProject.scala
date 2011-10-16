import sbt._
import java.io.File

class BeToProject(info: ProjectInfo) extends DefaultProject(info) {

  override def fork = Some(new ForkScalaRun {
    val (os, separator) = System.getProperty("os.name").split(" ")(0).toLowerCase match {
      case "linux" => "linux" -> ":"
      case "windows" => "windows" -> ";"
      case x => x -> ":"
    }

    override def runJVMOptions = {
      super.runJVMOptions ++ Seq("-Djava.library.path=" + System.getProperty("java.library.path") + separator + ("lib" / "native" / os))
    }

    override def scalaJars = Seq(buildLibraryJar.asFile, buildCompilerJar.asFile)
  })
}
