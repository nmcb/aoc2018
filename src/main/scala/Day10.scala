import scala.annotation.*
import scala.io.*
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def +(p: Pos): Pos = Pos(x + p.x, y + p.y)
    def -(p: Pos): Pos = Pos(x - p.x, y - p.y)
    def *(i: Int): Pos = Pos(x * i, y * i)
    def min(that: Pos): Pos = Pos(x min that.x, y min that.y)
    def max(that: Pos): Pos = Pos(x max that.x, y max that.y)


  case class Box(min: Pos, max: Pos):

    def union(that: Box): Box =
      Box(min min that.min, max max that.max)

    def area: Long =
      val delta = max - min
      delta.x.toLong * delta.y.toLong

  object Box:
    def apply(p: Pos): Box = Box(p, p)

    def bounding(poss: IterableOnce[Pos]): Box =
      poss.iterator.map(apply).reduce(_ union _)

  case class Point(position: Pos, velocity: Pos):
    def step: Point = Point(position + velocity, velocity)
    def step(t: Int): Point = Point(position + velocity * t, velocity)

  object Point:
    def fromString(s: String): Point =
      s.trim match
        case s"position=<$x, $y> velocity=<$vx, $vy>" =>
          Point(Pos(x.trim.toInt, y.trim.toInt), Pos(vx.trim.toInt, vy.trim.toInt))


  // https://en.wikipedia.org/wiki/Exponential_search
  def exponentialLower[A, B](f: A => B, min: A)(x: B)(using int: Integral[A], ord: Ordering[B]): (A, A) =
    val a0 = int.zero
    val a1 = int.one
    val a2 = int.fromInt(2)

    @tailrec
    def loop(lo: A, hi: A): (A, A) = if f(min + hi) >= x then (min + lo + a1, min + hi + a1) else loop(hi, a2 * hi)

    if f(min) < x then loop(a0, a1) else  (min, min + a1)

  // https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_leftmost_element
  def binaryLower[A, B](f: A => B, min: A, max: A)(x: B)(using int: Integral[A], ord: Ordering[B]): A =
    val a1 = int.one
    val a2 = int.fromInt(2)

    @tailrec
    def loop(lo: A, hi: A): A =
      if lo >= hi then
        lo
      else
        val mid = (hi - lo) / a2 + lo
        if f(mid) >= x then loop(lo, mid) else loop(mid + a1, hi)

    loop(min, max)

  def exponentialBinaryLower[A, B](f: A => B, min: A)(x: B)(using Integral[A], Ordering[B]): A =
    val (min2, max) = exponentialLower(f, min)(x)
    binaryLower(f, min2, max)(x)


  def boundingAreaPoints(points: Seq[Point]): Long =
     Box.bounding(points.map(_.position)).area

  def stepBoundingArea(points: Seq[Point], t: Int): Long =
    boundingAreaPoints(points.map(_.step(t)))

  def minimizePointsArea(points: Seq[Point]): (Seq[Point], Int) =
    def slope(t: Int): Long = stepBoundingArea(points, t + 1) - stepBoundingArea(points, t)

    val minSecond = exponentialBinaryLower(slope, 0)(0L)
    (points.map(_.step(minSecond)), minSecond)

  val points: List[Point] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Point.fromString)
      .toList

  val start1: Long = System.currentTimeMillis
  val (sky, answer2) = minimizePointsArea(points)

  extension (points: Seq[Point]) def asString: String =
    val positions     = points.map(_.position)
    val Box(min, max) = Box.bounding(positions)
    val positionsSet  = positions.toSet

    val sky = StringBuffer()
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        if (positionsSet.contains(pos)) sky.append('#') else sky.append('.')
      }
      sky.append('\n')
    }
    sky.toString

  println(s"Answer day $day part 1: ${sky.asString}[${System.currentTimeMillis - start1}ms]")
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
