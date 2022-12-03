package main

class day3 extends TestSuite:
  /** Split a string in half, perfectly.
    *
    * @param s
    *   string to split in half
    * @return
    *   pair of strings representing the first half and second half
    */
  def perfectHalves(s: String): (String, String) =
    val size = s.size
    require(size % 2 == 0, "string must be equally divisible")

    val midpoint = size / 2
    val (f, b) = s.zipWithIndex.partition(_._2 < midpoint)
    val front = String(f.map(_._1).toArray)
    val back = String(b.map(_._1).toArray)
    (front, back)

  /** Find a common set of items from a list of sequences
    *
    * @param xs
    *   variable list of sequences
    * @return
    *   set that is the intersection of all items
    */
  def common[A](xs: Seq[A]*): Set[A] =
    require(xs.size > 0, "must have at least one seq to work with")
    xs.map(_.toSet).reduce(_ intersect _)

  /** Group of elves carrying rucksacks that have not been inspected and badged
    * yet.
    */
  case class GroupInsecure(
      a: Rucksack,
      b: Rucksack,
      c: Rucksack
  ):
    /** Secure this group of elves */
    def toBadgedGroup: BadgedGroup =
      val overlap = common(a.items, b.items, c.items)

      require(
        overlap.size == 1,
        s"require exactly 1 common item to assign badge, got ${overlap}"
      )

      val badge = overlap.head.toBadge
      BadgedGroup(a, b, c, badge)

  object GroupInsecure:
    def parse(s: Seq[Rucksack]): GroupInsecure =
      require(s.size == 3, "must have exactly 3 rucksacks")
      GroupInsecure(s(0), s(1), s(2))
  end GroupInsecure

  /** A secure group of elves carrying rucksacks */
  case class BadgedGroup(
      a: Rucksack,
      b: Rucksack,
      c: Rucksack,
      badge: Badge
  )

  /** Badge indicating which secure item a group of elves is allwoed to carry */
  case class Badge(typ: Item):
    lazy val priority = typ.priority

  /** Item that goes into a Rucksack */
  case class Item(id: Char):
    lazy val priority: Int = Item.priorityMap(id)
    lazy val toBadge: Badge = Badge(this)
  object Item:
    lazy val priorityMap =
      val l = Range('a', 'z').inclusive
      val u = Range('A', 'Z').inclusive
      (l ++ u).zipWithIndex.map((c, i) => (c, i + 1)).toMap

    def parse(c: Char): Item =
      require(c.isLetter, "char must be a letter")
      Item(c)

    def parseSeq(s: String): Seq[Item] =
      s.map(Item.parse)
  end Item

  /**
    * Rucksack that an elf uses to carry items. The `front` and `back`
    * compartments should contain mutually exclusive items, but due to poor
    * planning, there's always going to be one miscategorized item.
    *
    * @param front items in the front
    * @param back items in the back
    */
  case class Rucksack(front: Seq[Item], back: Seq[Item]):
    lazy val items = front ++ back

    val miscategorized: Item =
      val set = front.toSet.intersect(back.toSet)
      require(
        set.size == 1,
        s"expected exactly one miscategorization per rucksack, got ${set}"
      )
      set.head

  object Rucksack:
    def parse(s: String): Rucksack =
      val (f, b) = perfectHalves(s)
      Rucksack(
        front = Item.parseSeq(f),
        back = Item.parseSeq(b)
      )

    def parseSeq(s: String): Seq[Rucksack] = s.trim
      .split("\n")
      .map(_.trim)
      .map(Rucksack.parse)
      .toSeq
  end Rucksack

  //
  // tests
  //

  lazy val ex = """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
  """

  lazy val input = read("day3.txt")

  def part1(input: String): Int = Rucksack
    .parseSeq(input)
    .map(_.miscategorized.priority)
    .sum

  test("part1") {
    assertEquals(part1(ex), 157)
    println(part1(input))
  }

  def part2(input: String) = Rucksack
    .parseSeq(input)
    .grouped(3)
    .map(GroupInsecure.parse)
    .map(_.toBadgedGroup.badge.priority)
    .sum

  test("part2") {
    assertEquals(part2(ex), 70)
    println(part2(input))
  }
