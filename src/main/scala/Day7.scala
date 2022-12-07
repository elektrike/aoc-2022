import util.readFileLines

import java.nio.file.DirectoryStream
import scala.annotation.tailrec

object Day7:

  case class File(name: String, size: Long)
  case class Directory(parent: Option[Directory],
                       name: String,
                       directories: List[Directory],
                       files: List[File]) { self =>

    def add(directory: Directory): Directory =
      copy(directories = directory :: directories)

    def add(file: File): Directory =
      copy(files = file :: files)

    def updateDirectories(): Directory =
      copy(directories = directories.map(_.copy(parent = Some(self))))

    def updateParent(): Directory =
      val parent = self.parent match
        case Some(parent) =>
          val removed = parent.directories.filter(_.name != name)
          Some(parent.copy(directories = self :: removed))
        case None         => self.parent

      copy(parent = parent)
  }

  def solution(): (Long, Long) =
    val terminalOutput = readFileLines("input/day7.txt")
    val fileSystem     = readFileSystem(terminalOutput)

    (partOne(fileSystem), partTwo(fileSystem))

  def partOne(fileSystem: Directory): Long =
    toDirectorySizes(fileSystem).values.filter(_.<=(100000)).sum

  def partTwo(fileSystem: Directory): Long =
    val directorySizes = toDirectorySizes(fileSystem)
    val missingSpace   = 30000000 - 70000000 - directorySizes("/")

    directorySizes.values.find(_.>(missingSpace)).head

  def readFileSystem(terminalOutput: List[String]): Directory =
    @tailrec
    def toMainDirectory(directory: Directory): Directory =
      if (directory.parent.isEmpty) directory else toMainDirectory(directory.parent.get)

    val fileSystem = terminalOutput
      .tail
      .foldLeft(Directory(None, "/", List.empty, List.empty)) { (currentDir, line) =>
        val dir = currentDir.updateParent().updateDirectories()

        line match
          case s"$$ cd ${name}"   => name match
            case ".." => dir.parent.get
            case _    => dir.directories.find(_.name == name).get
          case s"$$ ls"           => dir
          case s"dir ${name}"     => dir.add(Directory(Some(dir), name, List.empty, List.empty))
          case s"${size} ${name}" => dir.add(File(name, size.toInt))
      }

    toMainDirectory(fileSystem)

  def toDirectorySizes(filesystem: Directory): Map[String, Long] =
    @tailrec
    def fullPath(path: String, directory: Directory): String =
      directory.parent match
        case Some(parent) => fullPath("/".concat(parent.name).concat(path), parent)
        case _            => path

    var directorySizes: Map[String, Long] = Map.empty
    def directorySize(directory: Directory): Long =
      val size = directory.files.map(_.size).sum + directory.directories.map(directorySize).sum
      directorySizes = directorySizes.updated(fullPath(directory.name, directory), size)

      size
    directorySize(filesystem)

    directorySizes
