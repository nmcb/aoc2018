import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val (rules, plants) =
    val input = Source.fromResource(s"input$day.txt").getLines.toVector

    /** note that we only maintain pot indices that yield a plant */
    val plants = input(0) match
      case s"initial state: $pots" => pots.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

    /** note that we only maintain rules that yield a plant */
    val rules = input.drop(2).toSet.collect:
      case s"$pattern => $output" if output == "#" => pattern

    (rules, plants)

  extension (plants: Set[Int])

    def next(rules: Set[String]): Set[Int] =

      def surrounding(index: Int): String =
        (-2 to 2).map(i => if plants.contains(index + i) then '#' else '.').mkString

      (plants.min - 2 to plants.max + 2)
        .flatMap(pot => Option.when(rules.contains(surrounding(pot)))(pot))
        .toSet

  val start1  = System.currentTimeMillis
  val answer1 = Iterator.iterate(plants)(_.next(rules)).drop(20).next.sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /** utilises the fact that the problem converses */
  def solve2(plants: Set[Int], rules: Set[String], times: Long): Long =
    @tailrec
    def go(plants: Set[Int], conversion: Int, generation: Int): Long =
      val next = plants.next(rules)
      val delta = next.sum - plants.sum
      if delta == conversion then
        next.sum + delta * (times - generation)
      else
        go(next, delta, generation + 1)

    go(plants, 0, 1)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(plants, rules, times = 50000000000L)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")

