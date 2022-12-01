import util.readBatches

object Day1 {

  def solution(): (Int, Int) = {
    val inventory = toInventory("input/day1.txt")

    (partOne(inventory), partTwo(inventory))
  }

  def toInventory(path: String): List[Int] =
    readBatches(path).map(_.split("\n").map(_.toInt).sum)

  def partOne(inventory: List[Int]): Int =
    inventory.max

  def partTwo(inventory: List[Int]): Int =
    inventory.sortWith(_ > _).take(3).sum
}
