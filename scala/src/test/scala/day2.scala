package main

class day2 extends TestSuite:
  import Shape.*
  import Outcome.*
  import GameState.*

  enum GameState(val score: Int):
    case Win extends GameState(6)
    case Draw extends GameState(3)
    case Lose extends GameState(0)
  object GameState:
    def parse(s: String): GameState = s match
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
      case _ => throw IllegalArgumentException(s"cannot parse game state: ${s}")
  end GameState

  case class Outcome(state: GameState, shape: Shape):
    def score: Int = state.score + shape.score
  end Outcome

  enum Shape(val score: Int):
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissors extends Shape(3)

    def vs(other: Shape): GameState = this match
      case Rock =>
        other match
          case Rock     => Draw
          case Paper    => Lose
          case Scissors => Win

      case Paper =>
        other match
          case Rock     => Win
          case Paper    => Draw
          case Scissors => Lose

      case Scissors =>
        other match
          case Rock     => Lose
          case Paper    => Win
          case Scissors => Draw

    def stalematePlay: Shape = this
    def losingPlay: Shape = this match
      case Rock     => Scissors
      case Paper    => Rock
      case Scissors => Paper
    def winningPlay: Shape = this match
      case Rock     => Paper
      case Paper    => Scissors
      case Scissors => Rock

  object Shape:
    def parse(s: String): Shape = s match
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
      case _ => throw new IllegalArgumentException(s"unknown shape: ${s}")
  end Shape

  case class IncompleteStrategy(them: Shape, me: Shape):
    def play: Outcome = Outcome(me.vs(them), me)

  object IncompleteStrategy:
    def parse(s: String): IncompleteStrategy = s match
      case s"${a} ${b}" => IncompleteStrategy(Shape.parse(a), Shape.parse(b))
      case _ =>
        throw new IllegalArgumentException(s"could not parse strategy: ${s}")

    def parseList(s: String): List[IncompleteStrategy] =
      s.trim.split("\n").map(IncompleteStrategy.parse).toList

  end IncompleteStrategy

  case class TrueStrategy(them: Shape, desiredState: GameState):
    def play: Outcome =
      val myPlay = desiredState match
        case Win  => them.winningPlay
        case Draw => them.stalematePlay
        case Lose => them.losingPlay
      Outcome(desiredState, myPlay)

  object TrueStrategy:
    def parse(s: String): TrueStrategy = s match
      case s"${a} ${b}" =>
        TrueStrategy(Shape.parse(a), GameState.parse(b))
      case _ =>
        throw new IllegalArgumentException(s"could not parse strategy: ${s}")

    def parseList(s: String): List[TrueStrategy] =
      s.trim.split("\n").map(TrueStrategy.parse).toList

  end TrueStrategy

  //
  // tests
  //

  def part1(input: String): Int = IncompleteStrategy
    .parseList(input)
    .map(_.play)
    .map(_.score)
    .sum

  def part2(input: String): Int = TrueStrategy
    .parseList(input)
    .map(_.play)
    .map(_.score)
    .sum

  lazy val ex = """
A Y
B X
C Z
"""
  lazy val input = read("day2.txt")

  test("part1") {
    assertEquals(part1(ex), 15)
    println(part1(input))
  }

  test("part2") {
    assertEquals(part2(ex), 12)
    println(part2(input))
  }
