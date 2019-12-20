import java.io.File
import java.lang.Math.pow
import kotlin.math.sign

data class Coordinates(val row: Int, val column: Int) {
  operator fun plus(other: Coordinates) = Coordinates(row + other.row, column + other.column)
  operator fun minus(other: Coordinates) = Coordinates(row - other.row, column - other.column)
}

data class Portal(val name: Int, val steps: Int)

val PASSAGE = '.'
val WALL = '#'
val SPACE = ' '

val DIRECTIONS = listOf(
  Coordinates(0, 1),
  Coordinates(1, 0),
  Coordinates(0, -1),
  Coordinates(-1, 0)
)

tailrec fun calculateLeastSteps(
  currentList: List<Portal>,
  portals: Map<Int, List<Pair<Int, Int>>>,
  steps: Int = 1000000
): Int {
  if (currentList.isEmpty()) {
    return steps - 1
  } else {
    var nextSteps = steps
    val nextList = currentList.flatMap { node ->
      portals[-node.name]!!.mapNotNull {
        val nextPortal = Portal(it.first, it.second + node.steps + 1)
        if (nextPortal.steps < nextSteps) {
          if (nextPortal.name != 9090) {
            nextPortal
          } else {
            nextSteps = nextPortal.steps
            null
          }
        } else {
          null
        }
      }

    }
    return calculateLeastSteps(nextList, portals, nextSteps)
  }
}

fun findDistances(
  currentList: List<Pair<Coordinates, Coordinates>>,
  maze: Map<Coordinates, Char>,
  portals: Map<Coordinates, Int>,
  steps: Int = 0,
  currentDistances: List<Pair<Int, Int>> = emptyList()
): List<Pair<Int, Int>> {
  if (currentList.isEmpty()) {
    return currentDistances.filter { it.second != 0 && it.first != 6565 }
  } else {
    val nextDistances = mutableListOf<Pair<Int, Int>>()
    val nextList = currentList.flatMap { node ->
      val validSteps = DIRECTIONS.map { it + node.first }
        .filterNot { coord -> listOf(WALL, SPACE).any { it == maze.getValue(coord) } || coord == node.second }
      val nextPortal = validSteps.find { maze.getValue(it).isLetter() }
      (if (nextPortal == null) validSteps else {
        nextDistances.add(Pair(portals[node.first]!!, steps))
        validSteps - nextPortal
      }).map { it to node.first }
    }
    return findDistances(nextList, maze, portals, steps + 1, currentDistances + nextDistances)
  }
}

fun positivePortal(coordinates: Coordinates, height: Int, width: Int): Boolean {
  val validRows = listOf(2, height - 3)
  val validColumns = listOf(2, width - 3)
  return coordinates.row in validRows || coordinates.column in validColumns
}

fun portalDistances(
  maze: Map<Coordinates, Char>,
  portals: Map<Coordinates, Char>,
  height: Int,
  width: Int
): Map<Int, List<Pair<Int, Int>>> {
  val portalCoordinates = portals.map { l ->
    listOf(l, portals.filterKeys {
      it - l.key in DIRECTIONS
    }.entries.first()).sortedWith(compareBy({ it.key.column }, { it.key.row }))
  }.distinct().map { list ->
    val coordinates = list.flatMap { entry ->
      DIRECTIONS.map { it + entry.key }
    }.distinct().first { maze.getOrDefault(it, SPACE) == PASSAGE }
    val name = list.map { it.value.toInt() }.mapIndexed { idx, i ->
      i * pow(100.0, idx.toDouble()).toInt()
    }.sum()
    coordinates to if (positivePortal(coordinates, height, width)) name else -name
  }.toMap()

  return portalCoordinates.map {
    it.value to findDistances(listOf(Pair(it.key, it.key)), maze, portalCoordinates)
  }.toMap()
}

fun main() {
  val input = File("input").readLines()

  val height = input.size
  val width = input[0].length

  val maze = (0..height - 1).flatMap { row ->
    (0..width - 1).map { column ->
      Coordinates(row, column) to input[row][column]
    }
  }.toMap()

  val portals = maze.filterValues { it.isLetter() }
  val distances = portalDistances(maze, portals, height, width)

  val steps = calculateLeastSteps(listOf(Portal(-6565, 0)), distances)
  println(steps)
}

main()
