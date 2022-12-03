import util.readFileLines

object Day3:

  val toPriority: Map[Char, Int] = (('a' to 'z') ++ ('A' to 'Z') zip (LazyList from 1)).toMap

  def solution(): (Int, Int) =
    val rucksacks = readFileLines("input/day3.txt")

    (partOne(rucksacks), partTwo(rucksacks))

  def partOne(rucksacks: List[String]): Int =
    rucksacks
      .map(rucksack => rucksack.grouped(rucksack.length / 2).toList)
      .map(_.reduce(_ intersect _).head)
      .map(toPriority)
      .sum

  def partTwo(rucksacks: List[String]): Int =
    rucksacks
      .grouped(3)
      .map(_.reduce(_ intersect _).head)
      .map(toPriority)
      .sum
