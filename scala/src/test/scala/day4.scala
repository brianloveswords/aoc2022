package main

class day4 extends TestSuite:
  case class Assignment(start: Int, end: Int):
    require(start >= 1, "start must be >= 1")
    require(end <= 99, "end must be <= 99")
    require(start <= end, s"start must be <= end; got: ${start}, ${end}")

    def contains(other: Assignment): Boolean =
      other.start <= end
        && other.start >= start
        && other.end <= end

    def overlaps(other: Assignment): Boolean =
      (other.start >= start && other.start <= end)
        || (other.end <= end && other.end >= start)
        || (start >= other.start && start <= other.end)
        || (end <= other.end && end >= other.start)

    def viz: String =
      val chars = Range(1, 100).map { i =>
        if i >= start && i <= end
        then 'x'
        else '.'
      }.toArray
      String(chars)

  object Assignment:
    def parse(s: String): Assignment =
      val parts = s.trim.split('-').toSeq
      require(parts.size == 2, s"expected exactly two parts; got ${parts}")

      val ints = parts.map(_.trim.toInt)
      Assignment(
        start = ints(0),
        end = ints(1)
      )

  end Assignment

  case class Pair(a: Assignment, b: Assignment):
    def superset: Option[Assignment] =
      if a.contains(b) then Some(a)
      else if b.contains(a) then Some(b)
      else None

    def hasOverlap: Boolean =
      a.overlaps(b)

    def viz: String =
      s"${a.viz}\n${b.viz}\n\n"

  object Pair:
    def parse(s: String): Pair =
      val parts = s.trim.split(',').toSeq
      require(parts.size == 2, s"expected exactly 2 parts; got: ${parts}")

      val assignments = parts.map(Assignment.parse)
      Pair(assignments(0), assignments(1))

    def parseSeq(s: String): Seq[Pair] =
      s.trim.split('\n').map(parse).toSeq

  end Pair

  //
  // tests
  //

  lazy val ex = """
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
  """

  lazy val input = read("day4.txt")

  def part1(input: String) = Pair
    .parseSeq(input)
    .map(_.superset)
    .filter(_.isDefined)
    .size

  def part2(input: String) = Pair
    .parseSeq(input)
    .filter(_.hasOverlap)
    .size

  test("part1") {
    assertEquals(part1(ex), 2)
    println(part1(input))
  }

  test("part2") {
    assertEquals(part2(ex), 4)
    println(part2(input))
  }
