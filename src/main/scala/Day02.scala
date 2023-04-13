import scala.io.*

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Box(identifier: String):

    import Box.*

    private def idLettersWith(count: Int): Int =
      val calculation: State =
        identifier.foldLeft(InitState)((s,c) => s.updatedWith(c){
          case None          => Some(1)
          case Some(counter) => Some(counter + 1)
        })
      calculation.count((_, occurrences) => occurrences == count)

    val twoCharInId: Int =
      if idLettersWith(2) >= 1 then 1 else 0

    val threeCharInId: Int =
      if idLettersWith(3) >= 1 then 1 else 0

  object Box:

    val IdCharSet: String =
      "abcdefghijklmnopqrstuvwxyz"

    type State = Map[Char, Int]

    val InitState: State =
      Map.empty

    def fromString(id: String): Box =
      Box(id)


  val boxes: List[Box] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Box.fromString)
      .toList

  val answer1: Int =
    val twos   = boxes.foldLeft(0)((c,b) => c + b.twoCharInId)
    val threes = boxes.foldLeft(0)((c,b) => c + b.threeCharInId)
    twos * threes

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def differByOneCharInPlace(id1: String, id2: String): Boolean =
    id1.zip(id2).count(_ != _) == 1

  val answer2: String =
    val all: List[Option[(String,String)]] =
      boxes.map(box => {
        val found: Option[Box] =
          boxes
            .filterNot(_ == box)
            .find(other => differByOneCharInPlace(box.identifier, other.identifier))

        found match
          case None        => None
          case Some(other) => Some(box.identifier, other.identifier)
      })

    val (id1, id2) = all.filter(_.isDefined).head.getOrElse(sys.error("unable to find"))

    id2 intersect id1

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")

