import util.readBatches

object Day11:

  type WorryLevel = Long

  case class Test(divisibleBy: Int, ifTrue: Int, ifFalse: Int)

  case class Monkey(index: Int, items: List[WorryLevel], inspections: Int, operation: WorryLevel => WorryLevel, test: Test):
    def inspectItems: List[(Int, WorryLevel)] =
      items.map { worry =>
        val worryManaged = operation(worry)
        val toMonkey =
          if worryManaged % test.divisibleBy == 0
          then test.ifTrue
          else test.ifFalse

        (toMonkey, worryManaged)
      }

  object Monkey:
    def init(): Monkey = Monkey(0, List(), 0, old => old, Test(0, 0, 0))

  case class State(monkeys: List[Monkey]):
    self =>
    def playKeepAway: State = monkeys.indices.foldLeft(self)((state, index) => state.doRound(index))

    def doRound(index: Int): State =
      val itemsInspected    = monkeys(index).inspectItems
      val monkeysAfterThrow = monkeys.map(monkey =>
        if monkey.index == index
        then monkey.copy(inspections = monkey.inspections + itemsInspected.size, items = Nil)
        else monkey.copy(items = monkey.items ::: itemsInspected.filter(_._1 == monkey.index).map(_._2))
      )

      self.copy(monkeys = monkeysAfterThrow)

    val monkeyBusiness: Long = monkeys.map(_.inspections).sortWith(_ > _).take(2).reduce(_ * _)

  object State:
    def init(monkeys: List[Monkey]): State =
      State(monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) / 3)))

  def solution(): (Long, Long) =
    val input   = readBatches("input/day11.txt")
    val monkeys = input.map(_.split("\n").foldLeft(Monkey.init())(parseNotes))

    (partOne(monkeys, 20), 0)

  def parseNotes(monkey: Monkey, notes: String): Monkey =
    notes.trim match
      case s"Monkey ${index}:" => monkey.copy(index = index.toInt)
      case s"Starting items: ${items}" => monkey.copy(items = items.split(", ").map(_.toInt: WorryLevel).toList)
      case s"Operation: new = old ${operator} ${factor}" =>
        operator match
          case "+" => monkey.copy(operation = old => old + (if factor == "old" then old else factor.toLong))
          case "*" => monkey.copy(operation = old => old * (if factor == "old" then old else factor.toLong))
      case s"Test: divisible by ${number}" => monkey.copy(test = monkey.test.copy(divisibleBy = number.toInt))
      case s"If true: throw to monkey ${index}" => monkey.copy(test = monkey.test.copy(ifTrue = index.toInt))
      case s"If false: throw to monkey ${index}" => monkey.copy(test = monkey.test.copy(ifFalse = index.toInt))

  def partOne(monkeys: List[Monkey], rounds: Int): Long =
    LazyList.iterate(State.init(monkeys))(_.playKeepAway)(rounds).monkeyBusiness
