import scala.io.*
import scala.util.matching.*
import java.time.*

object Day04 extends App:

  val day: String = getClass.getName.drop(3).init

  enum Event:
    case BeginShift(guard: Int)
    case FallAsleep
    case WakeUp

  case class Record(timestamp: String, event: Event):
    val Timestamp: Regex = """\d{4}-\d{2}-\d{2} \d{2}:(\d{2})""".r
    def minute: Int =
      timestamp match
        case Timestamp(minute) => minute.toInt

  object Records:
    import Event.*

    val RecordLine: Regex = """\[(.{16})\] (.*)""".r
    val GuardShift: Regex = """Guard #(\d+) begins shift""".r

    def fromLines(input: List[String]): List[Record] =
      input
        .map {
          case RecordLine(timestamp, event) =>
            Record(timestamp, event match
              case GuardShift(guard) => BeginShift(guard.toInt)
              case "falls asleep"    => FallAsleep
              case "wakes up"        => WakeUp
            )
        }
        .toList
        .sortBy(_.timestamp)

  case class Shift(guard: Int, sleep: Set[Int])

  object Shift:

    def fromRecords(records: List[Record], shiftStarted: Option[Shift] = None, sleepStarted: Option[Int] = None): List[Shift] =
      records match
        case Nil =>
          shiftStarted.toList
        case record :: tl =>
          record.event match
            case Event.BeginShift(guard) =>
              shiftStarted.toList ++ fromRecords(tl, Some(Shift(guard, Set.empty)))
            case Event.FallAsleep =>
              fromRecords(tl, shiftStarted, Some(record.minute))
            case Event.WakeUp =>
              fromRecords(tl, shiftStarted.map(shift => shift.copy(sleep =  shift.sleep ++ (sleepStarted.get until record.minute).toSet)))

  trait Strategy:

    def choose(shifts: List[Shift]): Int

    def solve(lines: List[String]): Int =
      choose(Shift.fromRecords(Records.fromLines(lines)))

  object Strategy1 extends Strategy:
    def choose(shifts: List[Shift]): Int =
      val guardSleeps     = shifts.groupMapReduce(_.guard)(_.sleep.toSeq)(_ ++ _)
      val (guard, sleeps) = guardSleeps.maxBy((_, minutes) => minutes.length)
      val minute          = (0 until 60).maxBy(minute => sleeps.count(_ == minute))
      guard * minute

  object Strategy2 extends Strategy {
    override def choose(shifts: List[Shift]): Int = {
      val guardSleeps              = shifts.groupMapReduce(_.guard)(_.sleep.toSeq)(_ ++ _)
      val guardMinuteCount         = guardSleeps.view.mapValues(_.groupMapReduce(identity)(_ => 1)(_ + _)).toMap
      val minuteMaxGuardCount      = (0 until 60).map(minute => minute -> guardMinuteCount.view.mapValues(_.getOrElse(minute, 0)).maxBy((_, count) => count)).toMap
      val (minute, (guard, count)) = minuteMaxGuardCount.maxBy(_._2._2)
      guard * minute
    }
  }

  lazy val lines: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList


  val start1: Long = System.currentTimeMillis
  val answer1: Int = Strategy1.solve(lines)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Strategy2.solve(lines)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
