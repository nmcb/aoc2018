import scala.*
import io.*
import math.*

object Day06 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def manhattanDistance(p: Pos): Int =
      abs(x - p.x) + abs(y - p.y)

  type Coord = Pos
  object Coord:
    def apply(x: Int, y: Int): Coord = Pos(x, y)

  case class Grid(coordinates: List[Coord]):

    val minX: Int = coordinates.minBy(_.x).x
    val maxX: Int = coordinates.maxBy(_.x).x

    val minY: Int = coordinates.minBy(_.y).y
    val maxY: Int = coordinates.maxBy(_.y).y

    val positions: List[Pos] =
      (for { x <- minX to maxX ; y <- minY to maxY } yield Pos(x,y)).toList

    val closest: List[(Pos,Coord)] =
      positions.flatMap(p =>
        coordinates.map(c => (c,c.manhattanDistance(p))).sortBy((_,d) => d).take(2) match
          case (_,d0) :: (_,d1) :: _ if d0 == d1 => None
          case           (c, _) :: _             => Some(p,c)
          case                     _             => sys.error(s"no distance found for position: $p")
      )

    val areas: Map[Coord,Int] =
      closest.groupMapReduce((_,c) => c)((_,_) => 1)(_+_)

    def infinite(coordinate: Coord): Boolean =
      closest.exists((p,c) => c == coordinate && (p.x == minX || p.x == maxX || p.y == minY || p.y == maxY))

    val largestAreaSize: Int =
      val (_, size) = areas.filterNot((p,_) => infinite(p)).maxBy((_,s) => s)
      size

    def manhattanDistanceToAllCoordinates(position: Pos): Int =
      coordinates.map(position.manhattanDistance).sum

    def positionsWithManhattanDistanceToAllCoordinates(upperBound: Int): List[Pos] =
      positions.filter(p => manhattanDistanceToAllCoordinates(p) < upperBound)


  val coordinates: List[Coord] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map { case s"$x, $y" => Coord(x.toInt, y.toInt) }
        .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Grid(coordinates).largestAreaSize
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Grid(coordinates).positionsWithManhattanDistanceToAllCoordinates(10000).size
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
