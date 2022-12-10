package main

class day8 extends TestSuite:
  enum Direction:
    case Up
    case Down
    case Left
    case Right

  case class Position(x: Int, y: Int) extends Ordered[Position]:
    def move(direction: Direction): Position = direction match
      case Direction.Up    => up
      case Direction.Down  => down
      case Direction.Left  => left
      case Direction.Right => right

    lazy val up: Position = copy(y = y - 1)
    lazy val down: Position = copy(y = y + 1)
    lazy val left: Position = copy(x = x - 1)
    lazy val right: Position = copy(x = x + 1)

    def compare(that: Position): Int =
      if y == that.y
      then x - that.x
      else if y < that.y
      then -1
      else +1

  case class ViewPath(
      pos: Position,
      tree: Tree,
      up: Seq[Tree],
      down: Seq[Tree],
      left: Seq[Tree],
      right: Seq[Tree]
  ):
    lazy val scenicScore: Int =
      val height = tree.height
      def getScore(trees: Seq[Tree]): Int =
        val empty: Either[Int, Int] = Right(0)
        trees
          .foldLeft(empty) { (acc, next) =>
            acc.flatMap { (score) =>
              val newScore = score + 1
              if height > next.height
              then Right(newScore)
              else Left(newScore)
            }
          }
          .fold(id, id)

      getScore(up)
        * getScore(down)
        * getScore(left)
        * getScore(right)

    private def maxHeight(trees: Seq[Tree]): Int =
      trees.map(_.height).foldLeft(-1)(math.max)

    lazy val isVisible: Boolean =
      val height = tree.height

      height > maxHeight(up)
      || height > maxHeight(down)
      || height > maxHeight(left)
      || height > maxHeight(right)

    lazy val debug: String =
      def treeList(trees: Seq[Tree]): String = trees.map(_.height).mkString("")
      s"""
      |ViewPath: ${tree.height} @ (${pos.x}, ${pos.y})
      |     up: ${treeList(up)}
      |   down: ${treeList(down)}
      |   left: ${treeList(left)}
      |  right: ${treeList(right)}
      """.trim.stripMargin

    lazy val display: String =
      if isVisible
      then s"${tree.height}"
      else "."

  case class TreeGrid(grid: Map[Position, Tree]):
    private var posDirectionCache: Map[(Position, Direction), List[Tree]] =
      Map.empty

    private def getAllInDirection(
        pos: Position,
        direction: Direction
    ): List[Tree] = posDirectionCache.get((pos, direction)) match
      case Some(cached) => cached
      case None =>
        val nextPos = pos.move(direction)
        grid.get(nextPos) match
          case None => Nil
          case Some(tree) =>
            val result = tree :: getAllInDirection(nextPos, direction)
            posDirectionCache += (pos, direction) -> result
            result

    lazy val toViewPaths: Seq[ViewPath] =
      grid.toSeq.sortBy(_._1).map { (pos, tree) =>
        ViewPath(
          pos = pos,
          tree = tree,
          up = getAllInDirection(pos, Direction.Up),
          down = getAllInDirection(pos, Direction.Down),
          left = getAllInDirection(pos, Direction.Left),
          right = getAllInDirection(pos, Direction.Right)
        )
      }

    lazy val showCandidates: String =
      val paths = toViewPaths
      val mostScenicPos = paths.map(p => (p.pos, p.scenicScore)).maxBy(_._2)._1
      val result = paths
        .sortBy(_.pos)
        .groupBy(_.pos.y)
        .toSeq
        .sortBy(_._1)
        .map(_._2)
        .map(_.map { v =>
          if v.pos == mostScenicPos
          then ansiReset + ansiBold + ansiGreen + v.display + ansiReset + ansiDim
          else v.display
        }.mkString(""))
        .mkString("\n")
      ansiDim + result + ansiReset

  object TreeGrid:
    def parse(s: String): TreeGrid =
      val rows = s.trim.split('\n').map(_.trim).toSeq
      val result = rows.zipWithIndex.flatMap { (row, y) =>
        row.zipWithIndex.map { (h, x) =>
          Position(x, y) -> Tree.parse(h)
        }
      }
      TreeGrid(result.toMap)

  case class Tree(height: Int)
  object Tree:
    def parse(s: String): Tree = Tree(s.toInt)
    inline def parse(c: Char): Tree = parse(c.toString)

  lazy val example = """
30373
25512
65332
33549
35390
  """

  lazy val puzzle = read("day8.txt")

  test("parsing") {
    val result = TreeGrid.parse(example)
    val expected = TreeGrid(
      Map(
        // row 1
        Position(0, 0) -> Tree(3),
        Position(1, 0) -> Tree(0),
        Position(2, 0) -> Tree(3),
        Position(3, 0) -> Tree(7),
        Position(4, 0) -> Tree(3),
        // row 2
        Position(0, 1) -> Tree(2),
        Position(1, 1) -> Tree(5),
        Position(2, 1) -> Tree(5),
        Position(3, 1) -> Tree(1),
        Position(4, 1) -> Tree(2),
        // row 3
        Position(0, 2) -> Tree(6),
        Position(1, 2) -> Tree(5),
        Position(2, 2) -> Tree(3),
        Position(3, 2) -> Tree(3),
        Position(4, 2) -> Tree(2),
        // row 4
        Position(0, 3) -> Tree(3),
        Position(1, 3) -> Tree(3),
        Position(2, 3) -> Tree(5),
        Position(3, 3) -> Tree(4),
        Position(4, 3) -> Tree(9),
        // row 5
        Position(0, 4) -> Tree(3),
        Position(1, 4) -> Tree(5),
        Position(2, 4) -> Tree(3),
        Position(3, 4) -> Tree(9),
        Position(4, 4) -> Tree(0)
      )
    )
    assertEquals(result, expected)
  }

  test("position sorting") {
    val positions = Seq(
      Position(0, 1),
      Position(1, 1),
      Position(2, 1),
      Position(0, 0),
      Position(0, 2),
      Position(1, 2),
      Position(2, 2),
      Position(1, 0),
      Position(2, 0)
    )
    val expected = Seq(
      Position(0, 0),
      Position(1, 0),
      Position(2, 0),
      Position(0, 1),
      Position(1, 1),
      Position(2, 1),
      Position(0, 2),
      Position(1, 2),
      Position(2, 2)
    )
    val result = positions.sorted
    assertEquals(result, expected)
  }

  test("part1") {
    val grid = TreeGrid.parse(puzzle)
    val paths = grid.toViewPaths
    val result = paths.filter(_.isVisible).size
    val expected = 1763
    assertEquals(result, expected)
  }

  test("part2") {
    val grid = TreeGrid.parse(puzzle)
    val paths = grid.toViewPaths
    val result = paths.map(_.scenicScore).max
    val expected = 671160
    assertEquals(result, expected)
  }

  test("for fun") {
    val grid = TreeGrid.parse(puzzle)
    println(grid.showCandidates)
  }
