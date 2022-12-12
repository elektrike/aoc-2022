import util.readFileLines

import scala.collection.mutable

object Day12:

  case class Square(x: Int, y: Int):
    def existsWithin(heightmap: List[List[Int]]): Boolean =
      x >= 0 && x < heightmap.length && y >= 0 && y < heightmap.head.length

    def canReach(square: Square, heightmap: List[List[Int]]): Boolean =
      heightmap(x)(y) + 1 >= heightmap(square.x)(square.y)

  case class Node(square: Square, previous: Option[Node]):
    def stepsCounted(steps: Int): Int = previous match
      case None => steps
      case _ => previous.get.stepsCounted(steps + 1)

  val toElevation: Map[Char, Int] = Map('S' -> 0, 'E' -> 25) ++ (('a' to 'z') zip LazyList.from(0))

  val (horizontalStep, verticalStep) = (Array(-1, 0, 0, 1), Array(0, -1, 1, 0))

  var S: Square = Square(0, 0)
  var E: Square = Square(0, 0)
  var a: List[Square] = List()

  def solution(): (Int, Int) =
    val input = readFileLines("input/day12.txt")
    val heightmap = toHeightmap(input)

    (partOne(heightmap), partTwo(heightmap))

  def toHeightmap(input: List[String]): List[List[Int]] = input
    .zipWithIndex
    .map { (row, y) =>
      row.zipWithIndex.map { (mark, x) =>
        val square = Square(x, y)
        mark match
          case 'E' => E = square
          case 'S' => S = square
          case 'a' => a = a :+ square
          case _ =>

        toElevation(mark)
      }
    }.transpose

  def partOne(heightmap: List[List[Int]]): Int = hike(heightmap, List(S), E)

  def partTwo(heightmap: List[List[Int]]): Int = hike(heightmap, a, E)

  def hike(heightmap: List[List[Int]], startSquares: List[Square], goal: Square): Int =
    val visitedSquares = mutable.HashSet[String]()
    val queue = mutable.ArrayDeque[Node]()

    startSquares.foreach { square =>
      visitedSquares.addOne(square.x + ">" + square.y)
      queue.addOne(Node(square, None))
    }

    while queue.nonEmpty do
      val currentNode = queue.removeHead(true)
      val currentSquare = currentNode.square

      if currentSquare == goal
      then return currentNode.stepsCounted(0)

      for (i <- horizontalStep.indices)
        val nextSquare = Square(currentSquare.x + horizontalStep(i), currentSquare.y + verticalStep(i))

        if nextSquare.existsWithin(heightmap) && currentSquare.canReach(nextSquare, heightmap)
        then
          val nextNode = Node(nextSquare, Some(currentNode))
          val key = nextSquare.x + ">" + nextSquare.y

          if !visitedSquares.contains(key)
          then
            queue.addOne(nextNode)
            visitedSquares.addOne(key)
    0

