import java.io.File
import scala.util.matching.Regex

def recursiveListFiles(f: File, r: Regex): Array[File] = {
  val these = f.listFiles
  val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
  good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_, r))
}


def go() = {
  val f = new File("/Users/andrewscott/seniorDesign/Truckmuncher-Web/app/typings/app")
  val files = recursiveListFiles(f, new Regex(".*\\.ts"))
  files foreach { f =>
    val baseName = f.getPath.substring(0, f.getPath.indexOf(".ts"))
    val newFile = new File(baseName + ".d.ts")
    f.renameTo(newFile)
  }
}
