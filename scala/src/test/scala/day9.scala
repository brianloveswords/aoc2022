package main

class day9 extends TestSuite:

  enum Move(val n: Int):
    case Up(x: Int) extends Move(x)
    case Down(x: Int) extends Move(x)
    case Left(x: Int) extends Move(x)
    case Right(x: Int) extends Move(x)

    def toDirections: Seq[Direction] =
      import Direction.*
      val dir = this match
        case _: Up    => N
        case _: Down  => S
        case _: Left  => W
        case _: Right => E
      Seq.fill(n)(dir)
  object Move:
    def parse(s: String): Move =
      val parts = s.trim.split("\\s").toSeq
      require(parts.size == 2, s"should have found two parts, got: ${parts}")

      val dir = parts(0)
      val n = parts(1).toInt

      dir match
        case s"U" => Up(n)
        case s"D" => Down(n)
        case s"L" => Left(n)
        case s"R" => Right(n)
        case otherwise =>
          throw IllegalArgumentException(s"could not parse ${otherwise}")

    def parseSeq(s: String): Seq[Move] =
      s.trim.split("\n").map(parse).toSeq

  enum Equality:
    case Eq
    case Lt
    case Gt
  object Equality:
    def compare(a: Int, b: Int): Equality =
      if a == b
      then Eq
      else if a < b
      then Lt
      else Gt

  case class Pos(x: Int, y: Int):
    import Direction.*

    lazy val toUnit: Pos = Pos(x.sign, y.sign)

    def -(that: Pos): Pos = Pos(x - that.x, y - that.y)

    def stepTo(pos: Pos): Pos = move(directionTo(pos))

    def move(where: Direction): Pos = where match
      case X  => this
      case N  => Pos(x + 0, y + 1)
      case NE => Pos(x + 1, y + 1)
      case E  => Pos(x + 1, y + 0)
      case SE => Pos(x + 1, y - 1)
      case S  => Pos(x + 0, y - 1)
      case SW => Pos(x - 1, y - 1)
      case W  => Pos(x - 1, y + 0)
      case NW => Pos(x - 1, y + 1)

    def isTouching(that: Pos): Boolean =
      (this - that) == (this - that).toUnit

    /** what direction would something at this position have to move in order to
      * get closer to `that` position
      */
    def directionTo(that: Pos): Direction =
      import Equality.*
      val horizontal = compare(x, that.x)
      val vertical = compare(y, that.y)

      // both components are relative to `this`. if `horizontal=Lt`, that
      // means `this` is to the West of `that` and we would have to move in
      // the opposite direction (East) to get closer to `that`.
      (horizontal, vertical) match
        case (Eq, Eq) => X
        case (Eq, Lt) => N
        case (Eq, Gt) => S

        case (Lt, Eq) => E
        case (Lt, Lt) => NE
        case (Lt, Gt) => SE

        case (Gt, Eq) => W
        case (Gt, Lt) => NW
        case (Gt, Gt) => SW

  enum Direction:
    case X
    case N
    case NE
    case E
    case SE
    case S
    case SW
    case W
    case NW

  case class Rope(knots: Seq[Pos]):
    @annotation.targetName("moveAllMoves")
    def trail(moves: Seq[Move]): Seq[Rope] =
      val directions = moves.flatMap(_.toDirections)
      directions.scanLeft(this)(_ move _)

    @annotation.targetName("moveAllMoves")
    def moveAll(moves: Seq[Move]): Rope =
      moveAll(moves.flatMap(_.toDirections))

    @annotation.targetName("moveAllDirections")
    def moveAll(directions: Seq[Direction]): Rope =
      directions.foldLeft(this)(_ move _)

    def move(direction: Direction): Rope =
      val newHead = knots.head.move(direction)

      val zero: (List[Pos], Pos) = (List.empty, newHead)
      val (newTail, _) = knots.tail
        .foldLeft(zero) { (acc, currentPos) =>
          val (newKnots, lastPos) = acc
          val nextPos =
            if lastPos.isTouching(currentPos)
            then currentPos
            else currentPos.stepTo(lastPos)
          (nextPos :: newKnots, nextPos)
        }
      Rope(newHead :: newTail.reverse)

  object Rope:
    @annotation.targetName("applyVariadic")
    def apply(knots: Pos*): Rope =
      Rope(knots)

    @annotation.targetName("applyVariadicPos")
    def apply(knots: (Int, Int)*): Rope =
      Rope(knots.map(Pos.apply))

    def of(n: Int): Rope =
      Rope(Seq.fill(n)(Pos(0, 0)))

  //
  // tests
  //

  val example = """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
  """

  val largerExample = """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
  """

  val puzzle = read("day9.txt")

  test("part1") {
    val rope = Rope.of(2)
    val moves = Move.parseSeq(puzzle)
    val trail = rope.trail(moves)
    val tails = trail.map(_.knots(1))
    println(tails.toSet.size)
  }

  test("part2") {
    val rope = Rope.of(10)
    val moves = Move.parseSeq(puzzle)
    val trail = rope.trail(moves)
    val tails = trail.map(_.knots(9))
    println(tails.toSet.size)
  }
