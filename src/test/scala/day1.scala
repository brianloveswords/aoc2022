package main

// 2022-11-30 bjb: this is the 2021 day1 puzzle, used an example for the
// project skeleton. once the 2022 day1 puzzle is released I'll replace this
// with the real stuff.

class day1 extends TestSuite:
  case class Elf(id: Int, calories: Int)

  lazy val ex = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

  def parse(s: String): List[Elf] = s.trim
    .split("\n\n")
    .map(
      _.split("\n").toList
        .map(_.toInt)
        .sum
    )
    .toList
    .zipWithIndex
    .map((c, id) => Elf(id + 1, c))

  def part1(elves: List[Elf]): Elf =
    elves.maxBy(_.calories)

  def part2(elves: List[Elf]): Int =
    elves.sortBy(_.calories * -1).take(3).map(_.calories).sum

  test("part1") {
    val elves = parse(read("day1.txt"))
    assertEquals(part1(parse(ex)), Elf(4, 24000))
    println(part1(elves))
  }

  test("part2") {
    val elves = parse(read("day1.txt"))
    assertEquals(part2(parse(ex)), 45000)
    println(part2(elves))
  }
