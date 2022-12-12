import util.readBatches

object Day11:

  type WorryLevel = Long

  case class Test(divisibleBy: Int, ifTrue: Int, ifFalse: Int)

  case class Monkey(index: Int,
                    items: List[WorryLevel],
                    operation: WorryLevel => WorryLevel,
                    test: Test,
                    inspections: Long):
    def inspectItems: List[(Int, WorryLevel)] =
      items.map { worryLevel =>
        val worryLevelNew = operation(worryLevel)
        val throwTo =
          if   worryLevelNew % test.divisibleBy == 0
          then test.ifTrue
          else test.ifFalse

        (throwTo, worryLevelNew)
      }

  case class State(monkeys: List[Monkey]):
    self =>
    def playKeepAway: State = monkeys.indices.foldLeft(self)(_.doRound(_))

    def doRound(index: Int): State =
      val itemsInspected    = monkeys(index).inspectItems
      val monkeysAfterThrow = monkeys.map(monkey =>
        if   monkey.index == index
        then monkey.copy(inspections = monkey.inspections + itemsInspected.size, items = Nil)
        else monkey.copy(items = monkey.items ::: itemsInspected.filter(_._1 == monkey.index).map(_._2))
      )

      self.copy(monkeys = monkeysAfterThrow)

    val monkeyBusinessLevel: Long = monkeys.map(_.inspections).sortWith(_ > _).take(2).product

  object Monkey:
    def init(): Monkey = Monkey(0, List(), none => none, Test(0, 0, 0), 0)

  object State:
    def initPartOne(monkeys: List[Monkey]): State =
      State(monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) / 3)))

    def initPartTwo(monkeys: List[Monkey]): State =
      val divisor = monkeys.map(_.test.divisibleBy).product
      State(monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) % divisor)))

  def solution(): (Long, Long) =
    val input   = readBatches("input/day11.txt")
    val monkeys = input.map(_.split("\n").foldLeft(Monkey.init())(parseNotes))

    (partOne(monkeys), partTwo(monkeys))

  def parseNotes(monkey: Monkey, notes: String): Monkey =
    notes.trim match
      case s"Monkey ${index}:" =>
        monkey.copy(index = index.toInt)
      case s"Starting items: ${items}" =>
        monkey.copy(items = items.split(", ").map(_.toInt: WorryLevel).toList)
      case s"Operation: new = old ${operator} ${factor}" =>
        operator match
          case "+" => monkey.copy(operation = old => old + (if factor == "old" then old else factor.toLong))
          case "*" => monkey.copy(operation = old => old * (if factor == "old" then old else factor.toLong))
      case s"Test: divisible by ${number}" =>
        monkey.copy(test = monkey.test.copy(divisibleBy = number.toInt))
      case s"If true: throw to monkey ${index}" =>
        monkey.copy(test = monkey.test.copy(ifTrue = index.toInt))
      case s"If false: throw to monkey ${index}" =>
        monkey.copy(test = monkey.test.copy(ifFalse = index.toInt))

  def partOne(monkeys: List[Monkey]): Long =
    LazyList.iterate(State.initPartOne(monkeys))(_.playKeepAway)(20).monkeyBusinessLevel

  def partTwo(monkeys: List[Monkey]): Long =
    LazyList.iterate(State.initPartTwo(monkeys))(_.playKeepAway)(10000).monkeyBusinessLevel
