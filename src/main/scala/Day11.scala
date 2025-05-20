import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Cell = (Int,Int)

  val serial = 8561

  extension (cell: Cell)
    def x: Int = cell._1
    def y: Int = cell._2
    def rackId: Int = x + 10

    infix def +(that: Cell): Cell = (cell.x + that.x, cell.y + that.y)

    def powerLevel: Int =
      var level = cell.rackId * cell.y
      level = level + serial
      level = level * cell.rackId
      level = level.toString.dropRight(2).takeRight(1).toInt
      level - 5

    def asString: String = s"$x,$y"

  /** Given value table i(x,y) a summed area table can be computed as:
   *
   *  I(x,y) = i(x,y) + I(x-1,y) + I(x,y-1) - I(x-1,y-1)
   *
   *  After which the sum of an area can be computed as:
   *
   *  A(x0,y0,x1,y1) = I(x0,y0) + I(x1,y1) - I(x1,y0) - I(x0,y1)
   */

  /** (x,y) cells for 1 to 300 grid */
  val cells: Seq[Cell] =
    for y <- 1 to 300 ; x <- 1 to 300 yield (x,y)


  /** power level value table i(x,y) for given serial number */
  val grid: Map[Cell,Int] =
    cells.map(c => c -> c.powerLevel).toMap

  /** summed power level table I(x,y) for given value table */
  val table: Map[Cell,Int] =
    cells.foldLeft(Map.empty[Cell,Int].withDefaultValue(0)): (result, cell) =>
      result + (cell -> (grid(cell) + result(cell + (-1,0)) + result(cell + (0,-1)) - result(cell + (-1,-1))))

  /** area size, cell id and total power level for given summed power level table and area size */
  def area(size: Int): Seq[(Int,Cell,Int)] =
    val cells = for y <- size to 300 ; x <- size to 300 yield (x,y)
    cells
      .map: cell =>
        val total = table(cell) + table(cell + (-size,-size)) - table(cell + (-size,0)) - table(cell + (0,-size))
        (size, cell, total)

  extension (result: (Int,Cell,Int))
    def total: Int = result._3

  def solve1(): String =
    val (size, cell, total) = area(3).maxBy(_.total)
    (cell + (1 - size, 0) + (0, 1 - size)).asString

  val start1  = System.currentTimeMillis
  val answer1 = solve1()
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(): String =
    val (size, cell, total) = (for size <- 1 to 300 yield area(size)).flatten.maxBy(_.total)
    s"${(cell + (1 - size, 0) + (0, 1 - size)).asString},${size.toString}"

  val start2  = System.currentTimeMillis
  val answer2 = solve2()
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

