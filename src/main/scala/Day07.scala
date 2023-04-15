import scala.io.*
import scala.annotation.*
import scala.collection.immutable.SortedMap

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  def tsort[A](edges: List[(A, A)])(using Ordering[A]): List[A] =

    @tailrec
    def loop(deps: SortedMap[A, Set[A]], done: List[A] = List.empty): List[A] =
      val (nodeps, hasdeps) = deps.partition(_._2.isEmpty)
      if nodeps.isEmpty && hasdeps.isEmpty then done else
        val found  = nodeps.keys.headOption.getOrElse(sys.error("edges contains cycle"))
        val ignore = nodeps.drop(1)
        val next   = hasdeps.map((node,ds) => node -> (ds - found)) ++ ignore
        loop(next, done :+ found)

    loop(edges.foldLeft(SortedMap[A, Set[A]]())((ds, e) =>
      ds + (e._1 -> ds.getOrElse(e._1, Set())) + (e._2 -> (ds.getOrElse(e._2, Set()) + e._1))
    ))

  val steps: List[(Char,Char)] =

    def parser(s: String): (Char,Char) =
      s match
        case s"Step $fst must be finished before step $snd can begin." => (fst.head, snd.head)
        case _ => sys.error("boom!")

    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toList

  val start1: Long = System.currentTimeMillis
  val answer1: String = tsort(steps).mkString("")
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
