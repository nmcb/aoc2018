import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val frequencies: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  val answer1: Int = frequencies.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def firstFrequencyFoundTwice(frequencies: List[Int]): Int =
    assert(frequencies.length >= 2, "contains less than 2 frequencies")

    def find(fs: List[Int], seen: Set[Int] = Set(0), acc: Int = 0): Int =
      fs match
        case Nil                              => find(frequencies, seen, acc)
        case h :: _ if seen.contains(h + acc) => h + acc
        case h :: t                           => find(t, seen + (h + acc), h + acc)

    find(frequencies)

  val answer2: Int = firstFrequencyFoundTwice(frequencies)
    println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")

