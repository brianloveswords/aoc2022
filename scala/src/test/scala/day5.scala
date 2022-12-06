package main

class day5 extends TestSuite:
  import Model.*

  enum Model:
    case CrateMover9000
    case CrateMover9001

  case class Crate(id: Char)
  object Crate:
    def parse(s: String): Option[Crate] = s.trim match
      case ""         => None
      case s"[${id}]" => Some(Crate(id.charAt(0)))
      case otherwise =>
        throw IllegalArgumentException(s"invalid crate input: ${otherwise}")
  end Crate

  case class MoveInstruction(n: Int, from: Int, to: Int)
  object MoveInstruction:
    def parse(s: String): MoveInstruction = s.toLowerCase.trim match
      case s"move ${n} from ${from} to ${to}" =>
        MoveInstruction(n.toInt, from.toInt, to.toInt)
      case otherwise =>
        throw IllegalArgumentException(
          s"MoveInstruction.parse: didn't understand ${otherwise}"
        )

    def parseSeq(s: String): Seq[MoveInstruction] =
      s.trim.split('\n').map(parse).toSeq
  end MoveInstruction

  case class SupplyMessage(s: String)
  case class SupplyArea(area: Map[Int, Vector[Crate]]):
    lazy val message: SupplyMessage =
      val signature = String {
        area.toList
          .sortBy(_._1)
          .map(_._2)
          .map(_.lastOption)
          .collect { case Some(c) => c.id }
          .toArray
      }
      SupplyMessage(signature)

    def move(xs: Seq[MoveInstruction], model: Model): SupplyArea =
      xs.foldLeft(this) { (s, m) => s.move(m.n, m.from, m.to, model) }

    def move(n: Int, from: Int, to: Int, model: Model): SupplyArea =
      val crates = area(from).takeRight(n)
      require(
        crates.size == n,
        s"asked to move more crates than available; got n=${n} and crates=${crates}"
      )
      val orderedCrates = model match
        case CrateMover9000 => crates.reverse
        case CrateMover9001 => crates
      val newFromStack = area(from).dropRight(n)
      val newToStack = area(to).appendedAll(orderedCrates)
      val newArea = area.updated(from, newFromStack).updated(to, newToStack)
      SupplyArea(newArea)

  object SupplyArea:
    def apply(pairs: (Int, Vector[Crate])*): SupplyArea =
      SupplyArea(pairs.toMap)

    private val crateFinder = "(...) ?".r
    def parse(s: String) =

      // drop the line with the stack ids, we don't need it
      val clean = s.split('\n').filter(_.contains(']'))

      // figure out how many stacks we have. this works for the specific input
      // because there are never any completely empty stacks. this will be
      // used later to make stuff the same size so we can transpose.
      val maxStacks = clean.map(_.count(_ == ']')).max

      val result = clean
        .map(crateFinder.findAllIn(_).toVector)
        .filterNot(_.isEmpty)
        .map(_.map(Crate.parse).padTo(maxStacks, None))
        .toVector
        .transpose
        .map(_.collect { case Some(c) => c }.toVector)
        .zipWithIndex
        .map((v, i) => (i + 1, v.reverse))
        .toMap
      SupplyArea(result)
  end SupplyArea

  //
  // tests
  //

  val ex = """
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
  """

  val input = read("day5.txt")

  def prepare(input: String): (SupplyArea, Seq[MoveInstruction]) =
    val parts = input.split("\n\n")
    require(parts.size == 2, s"expected exactly 2 parts")

    val stacks = parts(0)
    val instructions = parts(1)
    (SupplyArea.parse(stacks), MoveInstruction.parseSeq(instructions))

  def part1(input: String): SupplyMessage =
    val (s, i) = prepare(input)
    s.move(i, CrateMover9000).message

  def part2(input: String): SupplyMessage =
    val (s, i) = prepare(input)
    s.move(i, CrateMover9001).message

  test("part 1") {
    assertEquals(part1(ex), SupplyMessage("CMZ"))
    println(part1(input))
  }

  test("part 1") {
    assertEquals(part2(ex), SupplyMessage("MCD"))
    println(part2(input))
  }
