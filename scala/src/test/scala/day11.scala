package main

class day11 extends TestSuite:
  import Operand.*
  import BinaryOp.*
  import DivisibleBy.*
  import WorryMode.*

  enum WorryMode:
    case Relaxed
    case Stressed(n: Int)

    def inspect(item: Item): Item = this match
      case Relaxed     => Item(item.worryLevel / 3)
      case Stressed(n) => Item(item.worryLevel % n)
  end WorryMode

  //

  case class Item(worryLevel: Long):
    def debug: String = worryLevel.toString

  object Item:
    def apply(n: Int): Item = new Item(n.toLong)

    def vec(xs: Int*): Vector[Item] = xs.map(Item.apply).toVector
    def parseInput(s: String): Vector[Item] =
      val parts = s.toLowerCase.split("items:").toSeq
      require(parts.size == 2, s"expected exactly 2 parts, got: ${parts}")
      parts(1)
        .split(",")
        .map(_.trim)
        .map(_.toInt)
        .map(Item.apply)
        .toVector

  //

  case class MonkeyId(id: Int)
  object MonkeyId:
    def parseLabel(s: String): MonkeyId = s.trim match
      case s"monkey ${id}:" => MonkeyId(id.toInt)
      case otherwise =>
        throw IllegalArgumentException(s"could not parse: ${otherwise}")

    def parseFromTestCase(s: String): (Boolean, MonkeyId) =
      s.toLowerCase.trim.replace("\\s+", " ") match
        case s"if ${bool}: throw to monkey ${id}" =>
          (bool.toBoolean, MonkeyId(id.toInt))
        case otherwise =>
          throw IllegalArgumentException(s"could not parse: ${otherwise}")

  //

  case class DivisibleBy(n: Int):
    def apply(other: Item): Boolean = other.worryLevel % n == 0
    def debug: String = s"divisible by ${n}"

  object DivisibleBy:
    def parseInput(s: String): DivisibleBy = s.toLowerCase.trim match
      case s"test: divisible by ${n}" => DivisibleBy(n.toInt)
      case otherwise =>
        throw IllegalArgumentException(s"cannot parse: ${otherwise}")

  end DivisibleBy

  //

  enum Operand:
    case Old
    case Number(n: Int)

    def fold(old: Long): Long = this match
      case Old       => old
      case Number(n) => n.toLong

    def debug: String = this match
      case Old       => "old"
      case Number(n) => s"${n}"

  object Operand:
    def parse(s: String): Operand = s.toLowerCase match
      case "old" => Old
      case n     => Number(n.toInt)
  end Operand

  //

  enum BinaryOp(val a: Operand, val b: Operand):
    case Add(a1: Operand, b1: Operand) extends BinaryOp(a1, b1)
    case Mult(a1: Operand, b1: Operand) extends BinaryOp(a1, b1)

    def debug: String =
      val op = this match
        case _: Add  => s"+"
        case _: Mult => s"*"
      s"${a.debug} ${op} ${b.debug}"

  object BinaryOp:
    def parse(s: String): BinaryOp =
      val parts = s.trim.split("\\s+").toSeq
      require(parts.size == 3, s"expected exactly 3 parts; got ${parts}")

      val a = Operand.parse(parts(0))
      val b = Operand.parse(parts(2))
      parts(1) match
        case "+" => Add(a, b)
        case "*" => Mult(a, b)

    def parseInput(s: String): BinaryOp =
      val parts = s.toLowerCase.trim.split("operation: new = ").toSeq
      require(parts.size == 2, s"expected 2 parts, got: ${parts}")
      parse(parts(1))

  end BinaryOp

  //

  case class TestCase(
      divisibleBy: DivisibleBy,
      ifTrue: MonkeyId,
      ifFalse: MonkeyId
  ):
    def apply(item: Item): MonkeyThrow =
      val monkeyId =
        if divisibleBy(item)
        then ifTrue
        else ifFalse
      MonkeyThrow(monkeyId, item)

    def debug: String =
      s"""${divisibleBy.debug}
      |    If true: throw to monkey ${ifTrue.id}
      |    If false: throw to monkey ${ifFalse.id}
      """.stripMargin.trim

  object TestCase:
    def parseParts(
        divisibleByLine: String,
        line1: String,
        line2: String
    ): TestCase =
      val divisibleBy = DivisibleBy.parseInput(divisibleByLine)
      val (bool1, id1) = MonkeyId.parseFromTestCase(line1)
      val (bool2, id2) = MonkeyId.parseFromTestCase(line2)
      val (trueId, falseId) = (bool1, bool2) match
        case (true, false) => (id1, id2)
        case (false, true) => (id2, id1)
        case otherwise =>
          throw IllegalArgumentException(
            s"illegal test case:\n${line1}\n${line2}"
          )
      TestCase(divisibleBy, trueId, falseId)

  end TestCase

  //

  case class MonkeyThrow(monkeyId: MonkeyId, item: Item)

  //

  case class Round(monkeys: Vector[Monkey]):
    lazy val monkeyBusiness: Long = monkeys
      .map(_.inspections.toLong)
      .sortBy(_ * -1)
      .take(2)
      .product

    private lazy val stressed: WorryMode =
      val value = monkeys
        .map(_.testCase.divisibleBy.n)
        .toSet
        .product
      Stressed(value)

    def relaxedRounds(n: Int): Round = advanceRounds(n, WorryMode.Relaxed)

    def stressedRounds(n: Int): Round = advanceRounds(n, stressed)

    private def advanceRounds(n: Int, worryMode: WorryMode): Round =
      require(n >= 0, "n must be >= 0")
      (0 until n).foldLeft(this)((acc, _) => acc.nextRound(worryMode))

    private def nextRound(worryMode: WorryMode): Round =
      def catchAll(
          monkeys: Vector[Monkey],
          tossList: List[MonkeyThrow]
      ): Vector[Monkey] = monkeys.map { m =>
        tossList.foldLeft(m)(_ catchItem _)
      }

      @annotation.tailrec
      def go(done: Vector[Monkey], remaining: Vector[Monkey]): Vector[Monkey] =
        remaining.headOption match
          case None => done
          case Some(m) =>
            val (nextM, tossList) = m.takeAllTurns(worryMode)
            val nextDone = catchAll(done, tossList) :+ nextM
            val nextRemaining = catchAll(remaining.drop(1), tossList)
            go(nextDone, nextRemaining)
      Round(go(Vector.empty, monkeys))

  object Round:
    def apply(xs: Monkey*): Round = Round(xs.toVector)

    def parse(s: String): Round =
      val monkeys = s.trim
        .split("\n\n")
        .map(Monkey.parse)
        .toVector
      Round(monkeys)

  end Round
  //

  case class Monkey(
      id: MonkeyId,
      items: Vector[Item],
      operation: BinaryOp,
      testCase: TestCase,
      inspections: Int = 0
  ):
    def catchItem(t: MonkeyThrow): Monkey =
      if t.monkeyId == id
      then copy(items = items.appended(t.item))
      else this

    private def inspectItem(item: Item, worryMode: WorryMode): Item =
      val worryLevel = item.worryLevel
      val a1 = operation.a.fold(worryLevel)
      val b1 = operation.b.fold(worryLevel)
      val nextLevel = operation match
        case Add(a, b)  => a1 + b1
        case Mult(a, b) => a1 * b1
      worryMode.inspect(Item(nextLevel))

    def takeAllTurns(worryMode: WorryMode): (Monkey, List[MonkeyThrow]) =
      val empty: List[MonkeyThrow] = List.empty
      val newInspections = items.size + inspections
      val tossList = items
        .foldLeft(empty) { (acc, item) =>
          val nextItem = inspectItem(item, worryMode)
          testCase(nextItem) :: acc
        }
        .reverse
      val newMonkey = copy(items = Vector.empty, inspections = newInspections)
      (newMonkey, tossList)

    def debug: String =
      val itemDebug = items.map(_.debug).mkString(", ")
      s"""
      |Monkey: ${id.id}
      |  Items: ${itemDebug}
      |  Operation: ${operation.debug}
      |  Test: ${testCase.debug}
      |  Inspections: ${inspections}
      """.stripMargin

  object Monkey:
    def parse(s: String): Monkey =
      val parts = s.toLowerCase.split("\n").toSeq
      require(parts.size == 6, s"expected exactly 6 parts: got ${parts}")

      val id = MonkeyId.parseLabel(parts(0))
      val items = Item.parseInput(parts(1))
      val operation = BinaryOp.parseInput(parts(2))
      val testCase = TestCase.parseParts(parts(3), parts(4), parts(5))

      Monkey(id, items, operation, testCase)

  end Monkey

  //
  //
  //

  lazy val example = """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
  """.trim

  val puzzle = read("day11.txt")

  test("Item.parseVec") {
    val input = "Starting items: 79, 98"
    val expected = Item.vec(79, 98)
    val result = Item.parseInput(input)
    assertEquals(result, expected)
  }

  test("DivisibleBy.parse") {
    val input = "  Test: divisible by 23"
    val expected = DivisibleBy(23)
    val result = DivisibleBy.parseInput(input)
    assertEquals(result, expected)
  }

  test("BinaryOp.parseVerbose") {
    val input = "  Operation: new = old * 19"
    val expected = Mult(Old, Number(19))
    val result = BinaryOp.parseInput(input)
    assertEquals(result, expected)
  }

  test("Monkey.parseVerbose") {
    val input = """
    |Monkey 0:
    |  Starting items: 79, 98
    |  Operation: new = old * 19
    |  Test: divisible by 23
    |    If true: throw to monkey 2
    |    If false: throw to monkey 3
    """.trim.stripMargin

    val expected = Monkey(
      id = MonkeyId(0),
      items = Item.vec(79, 98),
      operation = Mult(Old, Number(19)),
      testCase = TestCase(
        divisibleBy = DivisibleBy(23),
        ifTrue = MonkeyId(2),
        ifFalse = MonkeyId(3)
      )
    )
    val result = Monkey.parse(input)
    assertEquals(result, expected)
  }

  test("Round.parse") {
    val m0 = Monkey(
      id = MonkeyId(0),
      items = Item.vec(79, 98),
      operation = Mult(Old, Number(19)),
      testCase = TestCase(
        divisibleBy = DivisibleBy(23),
        ifTrue = MonkeyId(2),
        ifFalse = MonkeyId(3)
      )
    )
    val m1 = Monkey(
      id = MonkeyId(1),
      items = Item.vec(54, 65, 75, 74),
      operation = Add(Old, Number(6)),
      testCase = TestCase(
        divisibleBy = DivisibleBy(19),
        ifTrue = MonkeyId(2),
        ifFalse = MonkeyId(0)
      )
    )

    val m2 = Monkey(
      id = MonkeyId(2),
      items = Item.vec(79, 60, 97),
      operation = Mult(Old, Old),
      testCase = TestCase(
        divisibleBy = DivisibleBy(13),
        ifTrue = MonkeyId(1),
        ifFalse = MonkeyId(3)
      )
    )

    val m3 = Monkey(
      id = MonkeyId(3),
      items = Item.vec(74),
      operation = Add(Old, Number(3)),
      testCase = TestCase(
        divisibleBy = DivisibleBy(17),
        ifTrue = MonkeyId(0),
        ifFalse = MonkeyId(1)
      )
    )
    val expected = Round(m0, m1, m2, m3)
    val result = Round.parse(example)
    assertEquals(result, expected)
  }

  test("part1: example") {
    val round = Round.parse(example)
    val result = round.relaxedRounds(20).monkeyBusiness
    val expected = 10605L
    assertEquals(result, expected)
  }

  test("part1: puzzle") {
    val round = Round.parse(puzzle)
    val result = round.relaxedRounds(20).monkeyBusiness
    val expected = 62491L
    assertEquals(result, expected)
  }

  test("part2: example") {
    val round = Round.parse(example)
    val result = round.stressedRounds(10000).monkeyBusiness
    val expected = 2713310158L
    assertEquals(result, expected)
  }

  test("part2: puzzle") {
    val round = Round.parse(puzzle)
    val result = round.stressedRounds(10000).monkeyBusiness
    val expected = 17408399184L
    assertEquals(result, expected)
  }
