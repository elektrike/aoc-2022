import Day2.Hand.{Rock, Paper, Scissors}
import Day2.Outcome.{Lose, Draw, Win}
import util.readFileLines

object Day2:

  enum Hand:
    case Rock, Paper, Scissors

  enum Outcome:
    case Lose, Draw, Win

  val toHand = Map('A' -> Rock, 'B' -> Paper, 'C' -> Scissors, 'X' -> Rock, 'Y' -> Paper, 'Z' -> Scissors)

  val toOutcome = Map('X' -> Lose, 'Y' -> Draw, 'Z' -> Win)

  val beats = Map(Scissors -> Paper, Paper -> Rock, Rock -> Scissors)

  val toPoints = Map(Rock -> 1, Paper -> 2, Scissors -> 3, Lose -> 0, Draw -> 3, Win -> 6)

  def solution(): (Int, Int) =
    val guide = readFileLines("input/day2.txt")

    (partOne(guide), partTwo(guide))

  def partOne(guide: List[String]): Int =
    def decrypt(row: String): (Hand, Hand) =
      (toHand(row.head), toHand(row.last))

    guide.map(decrypt andThen play andThen score).sum

  def partTwo(guide: List[String]): Int =
    def decrypt(row: String): (Hand, Hand) =
      val opponentHand = toHand(row.head)
      val outcome      = toOutcome(row.last)

      (opponentHand, chooseHand(opponentHand, outcome))

    def chooseHand(opponentHand: Hand, outcome: Outcome): Hand =
      outcome match
        case Lose => beats(opponentHand)
        case Win  => beats.map(_.swap)(opponentHand)
        case Draw => opponentHand

    guide.map(decrypt andThen play andThen score).sum

  def play(hands: (Hand, Hand)): (Outcome, Hand) =
    val outcome =
      if      (hands._1 == hands._2)        Draw
      else if (hands._1 == beats(hands._2)) Win
      else                                  Lose

    (outcome, hands._2)

  def score(round: (Outcome, Hand)): Int =
    toPoints(round._1) + toPoints(round._2)
