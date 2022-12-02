import util.readBatches

object Day1:

  def solution(): (Int, Int) =
    val inventory = readBatches("input/day1.txt").map(sumCalories)

    (partOne(inventory), partTwo(inventory))

  def sumCalories(elfInventory: String): Int =
    elfInventory
      .split("\n")
      .map(_.toInt)
      .sum

  def partOne(inventory: List[Int]): Int =
    inventory.max

  def partTwo(inventory: List[Int]): Int =
    inventory
      .sortWith(_ > _)
      .take(3)
      .sum
