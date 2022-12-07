package main

class day6 extends TestSuite:
  def detect(input: String, window: Int): Int = input
    .sliding(window)
    .map(_.toSet.size)
    .zipWithIndex
    .find(_._1 == window)
    .map(_ + _)
    .getOrElse(
      throw new NoSuchElementException(
        s"could not find start-of-packet marker; window=${window}"
      )
    )

  lazy val input = read("day6.txt")

  test("part1") {
    val result = detect(input, window = 4)
    println(result)
  }

  test("part2") {
    val result = detect(input, window = 14)
    println(result)
  }
