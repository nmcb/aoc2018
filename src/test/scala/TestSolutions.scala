import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [26ms]") {
    assertResult(477)(actual = Day01.answer1)
    assertResult(390)(actual = Day01.answer2)
  }

  test("Day03 [15s]") {
    assertResult(107043)(actual = Day03.answer1)
    assertResult(346)(actual = Day03.answer2)
  }