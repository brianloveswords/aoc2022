package main

class day1 extends TestSuite:
  case class IncFlux(value: Int, previous: Int):
    def next(x: Int): IncFlux =
      if x > previous
      then IncFlux(value + 1, x)
      else IncFlux(value, x)

  def part1(xs: List[Int]): Int =
    val initialState = IncFlux(0, xs.head)
    val result = xs.tail.foldLeft(initialState)((acc, x) => acc.next(x))
    result.value

  def part2(xs: List[Int]): Int =
    val list = xs.sliding(3).map(_.sum).toList
    part1(list)

  lazy val ex = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  lazy val input = lines("day1.txt").map(_.toInt)

  test("part1") {
    assertEquals(part1(ex), 7)
    println(part1(input))
  }

  test("part2") {
    assertEquals(part2(ex), 5)
    println(part2(input))
  }
