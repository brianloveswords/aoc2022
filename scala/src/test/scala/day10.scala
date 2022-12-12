package main

class day10 extends TestSuite:

  enum Instruction:
    case Noop
    case AddX(n: Int)

    lazy val cycles = this match
      case Noop    => 1
      case AddX(_) => 2

    lazy val inc = this match
      case Noop    => 0
      case AddX(n) => n

  object Instruction:
    def parse(s: String): Instruction =
      val parts = s.trim.split("\\s+").toSeq
      require(parts.size > 0, "expected at least 1 part")

      parts(0) match
        case s"noop" => Noop
        case s"addx" =>
          require(
            parts.size == 2,
            s"expected exactly 2 parts for `addx`; got ${parts}"
          )
          AddX(parts(1).toInt)
        case otherwise =>
          throw new IllegalArgumentException(s"could not parse: ${otherwise}")

    def parseSeq(s: String): Seq[Instruction] =
      s.trim.split("\n").map(parse).toSeq

  import Instruction.*

  case class Machine(
      xStart: Int,
      xEnd: Int,
      cycle: Int
  ):
    lazy val signalStrength: Int =
      if Machine.broadcastRange.contains(cycle)
      then xStart * cycle
      else 0

    lazy val isSpriteVisible: Boolean =
      val pixel = (cycle - 1) % 40
      math.abs(xStart - pixel) <= 1

    lazy val draw: Char =
      if isSpriteVisible
      then '#'
      else '.'

    def run(inst: Instruction): Machine =
      val nextCycle = cycle + inst.cycles
      val nextEnd = xEnd + inst.inc
      Machine(xEnd, nextEnd, nextCycle)

    def interpolate(other: Machine): IndexedSeq[Machine] =
      def interp = Machine(xEnd, xEnd, _)
      val middle = Range(cycle + 1, other.cycle).map(interp)
      this +: middle

    override def toString: String = s"[|x=${xStart}•${xEnd} @${cycle}|]"

  object Machine:
    lazy val broadcastRange = Range(20, 220, step = 40).inclusive

    lazy val empty: Machine = Machine(
      xStart = 1,
      xEnd = 1,
      cycle = 0
    )

    def runAll(instructions: Seq[Instruction]): Seq[Machine] =
      val result = instructions.scanLeft(Machine.empty)(_ run _)

      def go(last: Machine, rem: Seq[Machine]): Seq[Machine] =
        rem.headOption match
          case None => Seq(last)
          case Some(head) =>
            if head.cycle - 1 == last.cycle
            then last +: go(head, rem.drop(1))
            else last.interpolate(head) ++ go(head, rem.drop(1))

      go(result.head, result.drop(1)).drop(1)

  //
  // tests
  //

  val puzzle = read("day10.txt")

  test("parse") {
    assertEquals(parse("noop "), Noop)
    assertEquals(parse("addx  -10"), AddX(-10))
  }

  test("parseSeq") {
    val input = """
    noop
    addx 3
    addx -5
    """
    val expected = Seq(Noop, AddX(3), AddX(-5))
    assertEquals(parseSeq(input), expected)
  }

  test("part1") {
    val instructions = parseSeq(puzzle)
    val cycles = Machine.runAll(instructions)
    val signalStrength = cycles.map(_.signalStrength).sum
    assertEquals(signalStrength, 11220)
  }

  test("part2") {
    val instructions = parseSeq(puzzle)
    val everyCycle = Machine.runAll(instructions)

    val result = everyCycle
      .map(_.draw)
      .grouped(40)
      .map(c => String(c.toArray))
      .mkString("\n")

    println(result)
  }
