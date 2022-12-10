import util.readFileLines

import scala.annotation.tailrec

object Day10:

  val interestingSignals: Seq[Int] = 20 to 240 by 40
  val crtRowEnds: Seq[Int] = 40 to 240 by 40

  def solution(): (Int, String) =
    val input  = readFileLines("input/day10.txt")
    val cycles = toCycles(input) zip (LazyList from 1)

    (partOne(cycles), partTwo(cycles))

  def partOne(cycles: List[(Int, Int)]): Int =
    @tailrec
    def execute(cycle: List[(Int, Int)], register: Int, signalStrengths: List[Int]): Int =
      cycle match
        case Nil => signalStrengths.sum
        case (value, cycleNum) :: tail =>
          val interestingSignalStrengths =
            if interestingSignals.contains(cycleNum)
            then register * cycleNum :: signalStrengths
            else signalStrengths

          execute(tail, register + value, interestingSignalStrengths)

    execute(cycles, 1, List[Int]())

  def partTwo(cycles: List[(Int, Int)]): String =
    @tailrec
    def draw(cycle: List[(Int, Int)], register: Int, crt: List[String]): String =
      cycle match
        case Nil => crt.mkString("\n")
        case (value, cycleNum) :: tail =>
          val pixel =
            if Range
              .inclusive(register - 1, register + 1)
              .contains(cycleNum - 1 - (crt.size - 1) * 40)
            then '#'
            else '.'

          val crtRendered = crt.init :+ crt.last + pixel
          val currentCrt  =
            if crtRowEnds.contains(cycleNum)
            then crtRendered.appended("")
            else crtRendered

          draw(tail, register + value, currentCrt)

    draw(cycles, 1, List(""))

  def toCycles(input: List[String]): List[Int] =
    input.flatMap {
      case s"addx ${value}" => List(0, value.toInt)
      case "noop"           => List(0)
    }
