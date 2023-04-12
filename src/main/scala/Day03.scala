import scala.io.*

object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Rec(id: Int, x0: Int, y0: Int, w: Int, h: Int):
    val x1: Int = x0 + w
    val y1: Int = y0 + h

    def contains(x: Int, y: Int): Boolean =
      (x >= x0) && (x < x1) && (y >= y0) && (y < y1)

    def overlap(that: Rec): Boolean =
      def loop(todo: List[(Int,Int)]): Boolean =
        todo match
          case Nil                          => false
          case (x,y) :: _ if contains(x, y) => true
          case _ :: t                       => loop(t)

      loop((that.x0 until that.x1).toList.flatMap(x => (that.y0 until that.y1).map(y => (x,y)).toList))


  object Rec:
    def fromString(l: String): Rec =
      l match
        case s"#${id} @ ${x},${y}: ${w}x${h}" => Rec(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
        case _ => sys.error(s"could not parse line: ${l}")

  val rectangles: List[Rec] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Rec.fromString)
      .toList

  def hasOverlap(rs: List[Rec])(x: Int, y: Int): Boolean =
    def loop(todo: List[Rec], foundFirst: Boolean = false): Boolean =
      todo match
        case Nil                                      => false
        case h :: _ if foundFirst && h.contains(x, y) => true
        case h :: t if h.contains(x, y)               => loop(t, foundFirst = true)
        case _ :: t                                   => loop(t, foundFirst)
    loop(rs)



  val answer1: Int =
    (0 until 1000).foldLeft(0)((a1,x) =>
      (0 until 1000).foldLeft(a1)((a0,y) =>
        if hasOverlap(rectangles)(x, y) then a0 + 1 else a0
      )
    )
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val answer2: Int =
    def loop(todo: List[Rec]): Rec =
      todo match
        case Nil                                                               => sys.error("boom!")
        case h :: t if rectangles.filterNot(_ == h).exists(_.overlap(h)) =>
          loop(t)
        case h :: _ =>
          h

    loop(rectangles).id
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")
