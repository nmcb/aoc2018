import scala.io.*
import scala.annotation.*
import scala.collection.immutable.SortedMap

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  def solve[A](edges: List[(A,A)], timer: A => Int, parallelization: Int)(using Ordering[A]): (Int,List[A]) =

    case class Work(queue: SortedMap[A,Int] = SortedMap(), workers: SortedMap[A,Int] = SortedMap()):

      def isDone: Boolean =
        queue.isEmpty && workers.isEmpty

      def tick(schedule: List[A]): (List[A], Work) =
        val work = queue ++ schedule.map(a => a -> timer(a))
        val (add, left)  = work.splitAt(parallelization - workers.size)
        val (done, todo) = (workers ++ add).map((c,t) => (c, t - 1)).partition((_,t) => t == 0)
        (done.keys.toList, Work(queue = left, workers = todo))

    @tailrec
    def loop(dependencies: SortedMap[A,Set[A]], time: Int = 0, work: Work = Work(), done: List[A] = List()): (Int, List[A]) =
      if dependencies.isEmpty && work.isDone then (time, done) else
        val (nodeps, hasdeps) = dependencies.partition((_,ds) => ds.isEmpty)
        val (processed, working) = work.tick(nodeps.keys.toList)
        val next = hasdeps.map((node,ds) => node -> (ds -- processed))
        loop(next, time + 1, working, done :++ processed)

    loop(edges.foldLeft(SortedMap[A,Set[A]]())((ds,e) =>
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
  val answer1: String = solve(steps, timer = _ => 1, parallelization = 1)._2.mkString("")
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  assert(answer1 == "FHICMRTXYDBOAJNPWQGVZUEKLS")


  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(steps, timer = _.toInt - 4, parallelization = 5)._1
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
  assert(answer2 == 946)
