import scala.annotation.*
import scala.collection.immutable.SortedMap
import scala.io.*

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Node(childs: List[Node], meta: List[Int]):
    def nrOfChilds: Int     = childs.size
    def nrOfMeta: Int       = meta.size
    def licenceNumber: Int  = meta.sum + childs.map(_.licenceNumber).sum
    def value: Int          = if childs.isEmpty then meta.sum else meta.map(idx => childs.lift(idx - 1).map(_.value).getOrElse(0)).sum

  object Node:

    def parseTree(s: String): Node =
      def loop(input: List[Int], todoChildren: Int, todoMeta: Int, children: List[Node] = List.empty): (Node, List[Int]) =
        if todoChildren == 0 then
          (Node(children, input.take(todoMeta)), input.drop(todoMeta))
        else
          // parse children
          val todoChildChildren = input.head
          val todoChildMeta = input.drop(1).head
          val (child, rest) = loop(input.drop(2), todoChildChildren, todoChildMeta, List.empty)
          loop(rest, todoChildren - 1, todoMeta, children :+ child)

      val input = s.trim.split(' ').map(_.toInt).toList
      val todoChildren = input.head
      val todoMeta = input.drop(1).head
      val (root, rest) = loop(input.drop(2), todoChildren, todoMeta)
      assert(rest.isEmpty)
      root


  val start1: Long = System.currentTimeMillis
  val answer1: Int = Node.parseTree(Source.fromResource(s"input$day.txt").mkString).licenceNumber
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  assert(answer1 == 47244)

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Node.parseTree(Source.fromResource(s"input$day.txt").mkString).value
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")
  assert(answer1 == 47244)
