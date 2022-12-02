import Day2.Hand.{Paper, Rock, Scissors}
import Day2.Outcome.{Lose, Draw, Win}
import util.readFileLines

object Day2:

  enum Hand:
    case Rock, Paper, Scissors

  enum Outcome:
    case Lose, Draw, Win

  val rules: Map[Hand, Hand] =
    Map(Scissors -> Paper, Paper -> Rock, Rock -> Scissors)

  val handDecrypted: Map[Char, Hand] =
    Map('A' -> Rock, 'B' -> Paper, 'C' -> Scissors,
        'X' -> Rock, 'Y' -> Paper, 'Z' -> Scissors)

  val outcomeDecrypted: Map[Char, Outcome] =
    Map('X' -> Lose, 'Y' -> Draw, 'Z' -> Win)

  val handScore: Map[Hand, Int] =
    Map(Rock -> 1, Paper -> 2, Scissors -> 3)

  val outcomeScore: Map[Outcome, Int] =
    Map(Lose -> 0, Draw -> 3, Win -> 6)

  def solution(): (Int, Int) =
    val guide = readFileLines("input/day2.txt")

    (partOne(guide), partTwo(guide))

  def partOne(guide: List[String]): Int =
    def decrypt(line: String): (Hand, Hand) =
      (handDecrypted(line.head), handDecrypted(line.last))

    guide
      .map(decrypt andThen play andThen score)
      .sum

  def partTwo(guide: List[String]): Int =
    def decrypt(line: String): (Hand, Hand) =
      val opponentHand = handDecrypted(line.head)
      val outcome = outcomeDecrypted(line.last)

      (opponentHand, chooseHand(opponentHand, outcome))

    guide
      .map(decrypt andThen play andThen score)
      .sum

  def chooseHand(opponentHand: Hand, outcome: Outcome): Hand =
    outcome match
      case Lose => rules(opponentHand)
      case Win => rules.map(_.swap)(opponentHand)
      case Draw => opponentHand

  def play(hands: (Hand, Hand)): (Outcome, Hand) =
    (resolveRound(hands), hands._2)

  def resolveRound(hands: (Hand, Hand)): Outcome =
    if (hands._1 == hands._2) Draw
    else if (hands._1 == rules(hands._2)) Win
    else Lose

  def score(round: (Outcome, Hand)): Int =
    outcomeScore(round._1) + handScore(round._2)
