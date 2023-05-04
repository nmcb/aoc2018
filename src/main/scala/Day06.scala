import scala.io.*

object Day06 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def manhattenDistance(p: Pos): Int =
      import math.*
      abs(x - p.x) + abs(y - p.y)

  case class Grid(ps: List[Pos]):

    val minX = ps.map(_.x).min
    val minY = ps.map(_.y).min
    val maxX = ps.map(_.x).max
    val maxY = ps.map(_.y).max

    val cells: List[Pos] =
      (for { x <- minX to maxX ; y <- minY to maxY } yield Pos(x, y)).toList

    val closest: Map[Pos,Pos] =
      cells.flatMap(c =>
        ps.map(p => ((c, p), p.manhattenDistance(c))).sortBy(_._2).take(2) match
          case ((_, _), d1) :: ((_, _), d2) :: _ if d1 == d2 => None
          case ((_, p), _)                  :: _             => Some(c, p)
          case                                 _             => sys.error("not distance found for: $c")
      ).toMap

    val areas: Map[Pos,Int] =
      closest.groupMapReduce((c,p) => p)((c,p) => 1)(_ + _)

    def infinite(l: Pos): Boolean =
      closest.exists((c,p) => p == l && (c.x == minX || c.x == maxX || c.y == minY || c.y == maxY))

    val largestAreaSize: Int =
      areas.toList.filterNot((p,_) => infinite(p)).maxBy((_, size) => size)._2

    def manhattenDistanceToAllPos(l: Pos): Int =
      ps.map(l.manhattenDistance).sum

    def cellsWithManhattenDistanceToAllPos(upperBound: Int): List[Pos] =
      cells.filter(l => manhattenDistanceToAllPos(l) < upperBound)


  val positions: List[Pos] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map{ case s"$x, $y" => Pos(x.toInt, y.toInt) }
        .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Grid(positions).largestAreaSize
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Grid(positions).cellsWithManhattenDistanceToAllPos(10000).size
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
