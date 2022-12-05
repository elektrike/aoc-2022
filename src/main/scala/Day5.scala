import Day5.CrateMover.{V9000, V9001}
import util.readFileLines

object Day5:

  enum CrateMover:
    case V9000, V9001

  case class Step(quantity: Int, from: Int, to: Int)

  def solution(): (String, String) =
    val input = readFileLines("input/day5.txt")
    val (stacks, procedure) = processInput(input)

    (partOne(stacks, procedure), partTwo(stacks, procedure))

  def processInput(input: List[String]): (List[String], List[Step]) =
    val (stacksInput, procedureInput) = input.splitAt(input.indexOf(""))

    val stacks = stacksInput
      .map(row => (1 to row.length by 4).map(row))
      .transpose
      .map(_.mkString.trim)

    val procedure = procedureInput
      .tail
      .map {
        case s"move ${amount} from ${from} to ${to}" =>
          Step(amount.toInt, from.toInt - 1, to.toInt - 1)
      }

    (stacks, procedure)

  def partOne(stacks: List[String], procedure: List[Step]): String =
    rearrange(stacks, procedure, V9000)

  def partTwo(stacks: List[String], procedure: List[Step]): String =
    rearrange(stacks, procedure, V9001)

  def rearrange(stacks: List[String], procedure: List[Step], crateMover: CrateMover): String =
    procedure
      .foldLeft(stacks)(moveCrates(_, _, crateMover))
      .map(_.head)
      .mkString

  def moveCrates(stacks: List[String], step: Step, crateMover: CrateMover): List[String] =
    val Step(quantity, from, to) = step

    val (cratesToMove, cratesRemaining) = stacks(from).splitAt(quantity)
    val cratesPickedUp = if (crateMover == V9001) cratesToMove.reverse else cratesToMove

    stacks
      .updated(from, cratesRemaining)
      .updated(to, cratesPickedUp + stacks(to))
