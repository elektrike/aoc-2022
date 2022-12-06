import util.readFileAsString

object Day6:

  def solution(): (Int, Int) =
    val datastream = readFileAsString("input/day6.txt")

    (partOne(datastream), partTwo(datastream))

  def partOne(datastream: String): Int =
    findMarkerStart(datastream, 4)

  def partTwo(datastream: String): Int =
    findMarkerStart(datastream, 14)

  def findMarkerStart(datastream: String, length: Int): Int =
    datastream.sliding(length).indexWhere(_.distinct.length == length) + length
