import scala.annotation.*
import scala.collection.immutable.SortedMap
import scala.io.*

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Node(childs: List[Node], meta: List[Long]):
    def nrOfChilds: Long     = childs.size
    def nrOfMeta: Long       = meta.size
    def licenceNumber: Long  = meta.sum + childs.map(_.licenceNumber).sum

  object Node:

    def parseTree(s: String): Node =
      def loop(input: List[Long], todoChildren: Long, todoMeta: Long, children: List[Node] = List.empty): (Node, List[Long]) =
        if todoChildren == 0 then
          (Node(children, input.take(todoMeta.toInt)), input.drop(todoMeta.toInt))
        else
          // parse children
          val todoChildChildren = input.head
          val todoChildMeta = input.drop(1).head
          val (child, rest) = loop(input.drop(2), todoChildChildren, todoChildMeta, List.empty)
          loop(rest, todoChildren - 1, todoMeta, children :+ child)

      val input = s.trim.split(' ').map(_.toLong).toList
      val todoChildren = input.head
      val todoMeta = input.drop(1).head
      val (root, rest) = loop(input.drop(2), todoChildren, todoMeta)
      assert(rest.isEmpty)
      root




  val start1: Long  = System.currentTimeMillis
  val answer1: Long = Node.parseTree(Source.fromResource(s"input$day.txt").mkString).licenceNumber
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  assert(answer1 == 47244)
