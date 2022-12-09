package main

type FSMap = Map[String, FSObject]

type FSPath = Vector[String]

enum FSObject:
  case File(size: Int)
  case Directory(contents: FSMap)

  lazy val totalSize: Int = this match
    case File(size) => size
    case Directory(contents) =>
      contents.values.foldLeft(0)((acc, o) => acc + o.totalSize)

import FSObject.*

case class Filesystem(root: Directory):
  def apply(n: String): FSObject = root.contents(n)
  lazy val totalSize = root.totalSize

  def addObject(
      parent: FSPath,
      name: String,
      obj: FSObject
  ): Filesystem =
    def go(stack: FSPath, cwd: Directory): Directory =
      if stack.isEmpty
      then Directory(cwd.contents + (name -> obj))
      else
        val next = stack.head
        cwd.contents(next) match
          case f: File =>
            throw IllegalArgumentException(
              s"not a directory: parent=${parent}, stack=${stack}, next=${next}"
            )
          case d: Directory =>
            Directory(cwd.contents + (next -> go(stack.drop(1), d)))
    Filesystem(go(parent, root))

  lazy val allDirectories: Vector[Directory] =
    def go(
        stack: Vector[FSObject],
        acc: Vector[Directory],
        n: Int
    ): Vector[Directory] =
      require(
        n < 1000,
        s"something is going wrong:\n\tstack=${stack}\n\tacc=${acc}"
      )

      if stack.isEmpty
      then acc
      else
        stack.head match
          case _: File => go(stack.drop(1), acc, n + 1)
          case d @ Directory(dir) =>
            go(
              stack = stack.drop(1) ++ dir.values,
              acc = acc.appended(d),
              n = n + 1
            )

    go(root.contents.values.toVector, Vector.empty, 0)

object Filesystem:
  import FSObject.*

  def empty = Filesystem(dir())

  def apply(objects: (String, FSObject)*): Filesystem =
    Filesystem(dir(objects*))

  def file(size: Int): File =
    File(size)

  def dir(objects: (String, FSObject)*): Directory =
    Directory(objects.toMap)

  def fromHistory(s: String): Filesystem =
    type CommandLine = String

    def go(
        history: Vector[CommandLine],
        cwd: FSPath,
        fs: Filesystem
    ): Filesystem =
      if history.isEmpty
      then fs
      else
        history.head match
          case s"$$ cd ${path}" =>
            val newCwd = path match
              case "/"  => Vector.empty
              case ".." => cwd.dropRight(1)
              case path => cwd.appended(path)
            go(history.drop(1), newCwd, fs)

          case s"$$ ls" =>
            go(history.drop(1), cwd, fs)

          case s"dir ${name}" =>
            val newFs = fs.addObject(cwd, name, dir())
            go(history.drop(1), cwd, newFs)

          case s"${size} ${name}" =>
            val newFs = fs.addObject(cwd, name, file(size.toInt))
            go(history.drop(1), cwd, newFs)
    go(
      history = s.trim.split('\n').toVector,
      cwd = Vector.empty,
      fs = Filesystem.empty
    )

class day7 extends TestSuite:
  val example = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
  """

  lazy val input = read("day7.txt")

  def part1(fs: Filesystem): Int = fs.allDirectories
    .map(_.totalSize)
    .filter(_ <= 100000)
    .sum

  def part2(fs: Filesystem): Int =
    val maxCapacity = 70000000
    val updateSize = 30000000
    val used = fs.totalSize
    val free = maxCapacity - used
    val needed = 30000000 - free
    fs.allDirectories
      .map(_.totalSize)
      .filter(_ >= needed)
      .sorted
      .head

  test("parsing") {
    import Filesystem.{file, dir}

    val result = Filesystem.fromHistory(example)
    val expected = Filesystem(
      "a" -> dir(
        "e" -> dir(
          "i" -> file(584)
        ),
        "f" -> file(29116),
        "g" -> file(2557),
        "h.lst" -> file(62596)
      ),
      "b.txt" -> file(14848514),
      "c.dat" -> file(8504156),
      "d" -> dir(
        "j" -> file(4060174),
        "d.log" -> file(8033020),
        "d.ext" -> file(5626152),
        "k" -> file(7214296)
      )
    )
    assertEquals(result, expected)
  }

  test("part1") {
    val fs = Filesystem.fromHistory(input)
    println(part1(fs))
  }

  test("part2") {
    val fs = Filesystem.fromHistory(input)
    println(part2(fs))
  }
