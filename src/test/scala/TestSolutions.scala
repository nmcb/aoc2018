import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [26ms]") {
    assertResult(477)(actual = Day01.answer1)
    assertResult(390)(actual = Day01.answer2)
  }