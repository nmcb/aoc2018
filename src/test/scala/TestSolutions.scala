import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [26ms]") {
    assertResult(477)(actual = Day01.answer1)
    assertResult(390)(actual = Day01.answer2)
  }
  test("Day02 [130ms]") {
    assertResult(5368)(actual = Day02.answer1)
    assertResult("cvgywxqubnuaefmsljdrpfzyi")(actual = Day02.answer2)
  }
  ignore("Day03 [15s]") {
    assertResult(107043)(actual = Day03.answer1)
    assertResult(346)(actual = Day03.answer2)
  }
  test("Day04 [6ms]") {
    assertResult(103720)(actual = Day04.answer1)
    assertResult(110913)(actual = Day04.answer2)
  }
  test("Day05 [101ms]") {
    assertResult(11310)(actual = Day05.answer1)
    assertResult(6020)(actual = Day05.answer2)
  }
  test("Day06 [1644ms]") {
    assertResult(2906)(actual = Day06.answer1)
    assertResult(50530)(actual = Day06.answer2)
  }
  test("Day07 [39ms]") {
    assertResult("FHICMRTXYDBOAJNPWQGVZUEKLS")(actual = Day07.answer1)
    assertResult(946)(actual = Day07.answer2)
  }
  test("Day08 [120ms]") {
    assertResult(47244)(actual = Day08.answer1)
    assertResult(17267)(actual = Day08.answer2)
  }
