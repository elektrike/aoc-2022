import Day9.Direction.{U, D, R, L}
import util.readFileLines

object Day9:

  enum Direction:
    case U, D, L, R

  case class Position(x: Int, y: Int):
    self =>
    val move: Direction => Position =
      case U => Position(x, y + 1)
      case D => Position(x, y - 1)
      case R => Position(x + 1, y)
      case L => Position(x - 1, y)

    def follow(knot: Position): Position =
      if isTouching(knot)
        then self
      else self.copy(coordinate(x, knot.x), coordinate(y, knot.y))

    def isTouching(knot: Position): Boolean =
      (x - knot.x).abs <= 1 && (y - knot.y).abs <= 1

    def coordinate(c1: Int, c2: Int): Int =
      if c1 < c2
        then c1 + 1
      else if c1 > c2
        then c1 - 1
      else c1

  case class State(knots: List[Position], visited: Set[Position]):
    self =>
    def moveHead(direction: Direction): State =
      self.copy(knots = knots.updated(0, knots.head.move(direction)))

    def moveTailKnot(index: Int): State =
      self.copy(knots = knots.updated(index, knots(index).follow(knots(index - 1))))

  object State:
    def init(knotCount: Int): State =
      State(List.fill(knotCount)(Position(0, 0)), Set(Position(0, 0)))

  def solution(): (Int, Int) =
    val directions = readFileLines("input/day9.txt").flatMap(toDirections)

    (partOne(directions), partTwo((directions)))

  def toDirections(motion: String): List[Direction] =
    val Array(direction, steps) = motion.split(" ")

    List.fill(steps.toInt)(Direction.valueOf(direction))

  def partOne(directions: List[Direction]): Int =
    directions.foldLeft(State.init(2))(applyDirection).visited.size

  def partTwo(directions: List[Direction]): Int =
    directions.foldLeft(State.init(10))(applyDirection).visited.size

  def applyDirection(state: State, direction: Direction): State =
    val tailRange = 1 to state.knots.tail.size
    val headMoved = state.moveHead(direction)

    val knotsMoved: State = tailRange.foldLeft(headMoved)((state, knotIndex) =>
      state.moveTailKnot(knotIndex)
    )

    knotsMoved.copy(visited = knotsMoved.visited + knotsMoved.knots.last)
