import util.readFileLines

object Day8:

  def solution(): (Int, Int) =
    val input = readFileLines("input/day8.txt")
    val treeMap = input.map(_.split("").map(_.toInt).toList)

    (partOne(treeMap), partTwo(treeMap))

  def partOne(treeMap: List[List[Int]]): Int =
    val visible = for {
      (treeRow, x) <- treeMap.zipWithIndex
      (tree,    y) <- treeRow.zipWithIndex
    } yield {
      val top    = (0 until x).forall(treeMap(_)(y) < tree)
      val bottom = (x + 1 until treeMap.length).forall(treeMap(_)(y) < tree)
      val left   = (0 until y).forall(treeMap(x)(_) < tree)
      val right  = (y + 1 until treeRow.length).forall(treeMap(x)(_) < tree)

      if (top || bottom || left || right) 1 else 0
    }

    visible.sum

  def partTwo(treeMap: List[List[Int]]): Int =
    val scenic = for {
      (treeRow, x) <- treeMap.zipWithIndex
      (tree,    y) <- treeRow.zipWithIndex
    } yield {
      val top = (0 until x)
        .findLast(treeMap(_)(y) >= tree)
        .map(x - _)
        .getOrElse(x)

      val bottom = (x + 1 until treeMap.length)
        .find(treeMap(_)(y) >= tree)
        .map(_ - x)
        .getOrElse((treeMap.length - 1) - x)

      val left = (0 until y)
        .findLast(treeMap(x)(_) >= tree)
        .map(y - _)
        .getOrElse(y)

      val right = (y + 1 until treeRow.length)
        .find(treeMap(x)(_) >= tree)
        .map(_ - y)
        .getOrElse((treeRow.length - 1) - y)

      top * bottom * left * right
    }

    scenic.max