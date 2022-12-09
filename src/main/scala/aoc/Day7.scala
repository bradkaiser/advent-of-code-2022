package aoc


object Day7 {

  type Path = Vector[String]

  trait Node
  case class Dir(path: Path) extends Node
  case class File(name: String, size: Int) extends Node

  trait TerminalLine
  case class Cd(dir: String) extends TerminalLine
  case object CdUp extends TerminalLine
  case object Ls extends TerminalLine
  case class DirLine(name: String) extends TerminalLine
  case class FileLine(size: Int, name: String) extends TerminalLine

  val cdR = """\$ cd (.+)""".r
  val cdUpR = """\$ cd \.\.""".r
  val lsR = """\$ ls""".r
  val dirR = """dir (\w+)""".r
  val fileR = """(\d+) (.+)""".r

  def parseLine(s: String): TerminalLine = s match {
    case cdUpR() => CdUp
    case cdR(name) => Cd(name)
    case dirR(name) => DirLine(name)
    case fileR(size, name) => FileLine(size.toInt, name)
    case lsR() => Ls
  }

  case class State(fs: Map[Dir, List[Node]], currentDir: Dir)

  def build(lines: List[TerminalLine]) = {
    val start = State(Map.empty[Dir, List[Node]], Dir(Vector.empty))

    val end = lines.foldLeft(start) { case (state, line) =>
      line match {
        case Cd(name) => state.copy(currentDir = Dir(state.currentDir.path.appended(name)))
        case CdUp => state.copy(currentDir = Dir(state.currentDir.path.dropRight(1)))
        case DirLine(name) =>
          val contents = Dir(state.currentDir.path.appended(name)) :: state.fs.getOrElse(state.currentDir, List.empty)
          state.copy(fs = state.fs + (state.currentDir -> contents))
        case FileLine(size, name) =>
          val contents = File(name, size) :: state.fs.getOrElse(state.currentDir, List.empty)
          state.copy(fs = state.fs + (state.currentDir -> contents))
        case Ls => state
      }
    }
    end.fs
  }

//  def sumDirs(fs: Map[Dir, List[Node]]) = {
//    val sizes: Map[Dir, Int] = Map.empty
//    def sumDir(dir: Dir, fs: Map[Dir, List[Node]], sizes: Map[Dir, Int]) = {
//      fs.g
//    }
//    fs.keys
//
//  }



}
