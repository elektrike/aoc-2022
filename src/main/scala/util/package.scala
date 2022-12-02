import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

package object util:

  def readFileLines(path: String): List[String] =
    Files.readAllLines(Paths.get(path)).asScala.toList

  def readBatches(path: String): List[String] =
    readFileAsString(path).split("\n\n").toList

  def readFileAsString(path: String): String =
    new String(Files.readAllBytes(Paths.get(path)))
