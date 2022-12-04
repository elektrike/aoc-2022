import util.readFileLines

import scala.collection.immutable.Range.Inclusive

object Day4:

  def solution(): (Int, Int) =
    val pairs = readFileLines("input/day4.txt")

    (partOne(pairs), partTwo(pairs))

  def partOne(pairs: List[String]): Int =
    pairs
      .map(toRanges)
      .count { ranges =>
        val intersection = toIntersection(ranges)

        ranges.exists(_ == intersection)
      }

  def partTwo(pairs: List[String]): Int =
    pairs
      .map(toRanges andThen toIntersection)
      .count(_.nonEmpty)

  def toRanges(pair: String): Array[Range] =
    pair
      .split(",")
      .map(_.split("-").map(_.toInt))
      .map(range => range.head to range.last)

  def toIntersection(ranges: Array[Range]): IndexedSeq[Int] =
    ranges.reduce(_ intersect _)
