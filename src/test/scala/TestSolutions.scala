import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(477)(actual = Day01.answer1)
    assertResult(390)(actual = Day01.answer2)

  test("Day02"):
    assertResult(5368)(actual = Day02.answer1)
    assertResult("cvgywxqubnuaefmsljdrpfzyi")(actual = Day02.answer2)

  ignore("Day03"):
    assertResult(107043)(actual = Day03.answer1)
    assertResult(346)(actual = Day03.answer2)

  test("Day04"):
    assertResult(103720)(actual = Day04.answer1)
    assertResult(110913)(actual = Day04.answer2)

  test("Day05"):
    assertResult(11310)(actual = Day05.answer1)
    assertResult(6020)(actual = Day05.answer2)

  test("Day06"):
    assertResult(2906)(actual = Day06.answer1)
    assertResult(50530)(actual = Day06.answer2)

  test("Day07"):
    assertResult("FHICMRTXYDBOAJNPWQGVZUEKLS")(actual = Day07.answer1)
    assertResult(946)(actual = Day07.answer2)

  test("Day08"):
    assertResult(47244)(actual = Day08.answer1)
    assertResult(17267)(actual = Day08.answer2)

  test("Day09"):
    assertResult(412117)(actual = Day09.answer1)
    assertResult(3444129546L)(actual = Day09.answer2)

  test("Day10"):
    assertResult(
      """######..#####....####...#....#..#.........##.......###..#.....
        |#.......#....#..#....#..#....#..#........#..#.......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#####...#####...#.........##....#.......#....#......#...#.....
        |#.......#..#....#.........##....#.......######......#...#.....
        |#.......#...#...#........#..#...#.......#....#......#...#.....
        |#.......#...#...#........#..#...#.......#....#..#...#...#.....
        |#.......#....#..#....#..#....#..#.......#....#..#...#...#.....
        |######..#....#...####...#....#..######..#....#...###....######
        |""".stripMargin)(actual = Day10.sky.asString)
    assertResult(10813)(actual = Day10.answer2)

