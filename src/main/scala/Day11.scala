import scala.annotation.*
import scala.io.*
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Day11 extends App:

  val day: String =
    getClass.getName.drop(3).init

  case class Cell():

    private def rackId(x: Int): Int =
      x + 10

    def powerLevel(x: Int, y: Int)(serial: Int): Int =
      val l0 = rackId(x) * y
      val l1 = l0 + serial
      val l2 = l1 * rackId(x)
      val l3 = l2 / 100 % 10
      val l4 = l3 - 5
      l4

  object Cell:
    def unit: Cell = Cell()

  assert(Cell.unit.powerLevel(x =   3, y =   5)(serial =  8) ==  4)
  assert(Cell.unit.powerLevel(x = 122, y =  79)(serial = 57) == -5)
  assert(Cell.unit.powerLevel(x = 217, y = 196)(serial = 39) ==  0)
  assert(Cell.unit.powerLevel(x = 101, y = 153)(serial = 71) ==  4)


  case class Grid(underlying: Grid.Square):

//    def id: (Int, Int) =
//      (s.map(_.x).min, s.map(_.y).min)
//
//    def totalPower(serial: Int): Int =
//      s.map(_.powerLevel(serial)).sum
//
//    val cells: Grid.Square =
//      Vector.tabulate(sizeX, sizeY)((x,y) => Cell().powerLevel(x + 1, y + 1))
//
//    def squareById(cell: Cell): Set[Cell] =
//      def rangeX: Range = Range.inclusive(cell.x, cell.x + 2)
//      def rangeY: Range = Range.inclusive(cell.y, cell.y + 2)
//      cells.filter(c => rangeX.contains(c.x) && rangeY.contains(c.y))
//
//    val squares: Set[Square] =
//      cells.map(c => { println(c) ; squareById(c) }).filter(_.size == 9)

    def largestSquarePowerLevel(serial: Int): Int =
      val blah =
        underlying
        .zipWithIndex
        .foldLeft(0)((level, y) => row)
//          .zipWithIndex
//          .map((cell, x)) => cell
//            .powerLevel(x, y)(serial))

//      squares.groupMapReduce(_.id)(_.totalPower(serial))(_ + _).maxBy(_._2)._1
      blah

  object Grid:

    type Square = Vector[Vector[Cell]]

    def of(sizeX: Int, sizeY: Int): Grid =
      Grid(Vector.fill(sizeX, sizeY)(Cell.unit))


  val start1: Long =
    System.currentTimeMillis

  val SerialNumber: Int =
    8561

  val answer1: String =
    val level =
      Grid
        .of(300, 300)
        .largestSquarePowerLevel(serial = SerialNumber)

    f"%%d, %%d".format(level)

  println(s"Answer day $day part 1: ${answer1}[${System.currentTimeMillis - start1}ms]")
