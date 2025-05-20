import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(477)(Day01.answer1)
    assertResult(390)(Day01.answer2)

  test("Day02"):
    assertResult(5368)(Day02.answer1)
    assertResult("cvgywxqubnuaefmsldjrpfzyi")(Day02.answer2)

  test("Day03"):
    assertResult(107043)(Day03.answer1)
    assertResult(346)(Day03.answer2)

  test("Day04"):
    assertResult(103720)(Day04.answer1)
    assertResult(110913)(Day04.answer2)

  test("Day05"):
    assertResult(11310)(Day05.answer1)
    assertResult(6020)(Day05.answer2)

  test("Day06"):
    assertResult(2906)(Day06.answer1)
    assertResult(50530)(Day06.answer2)

  test("Day07"):
    assertResult("FHICMRTXYDBOAJNPWQGVZUEKLS")(Day07.answer1)
    assertResult(946)(Day07.answer2)

  test("Day08"):
    assertResult(47244)(Day08.answer1)
    assertResult(17267)(Day08.answer2)

  test("Day09"):
    assertResult(412117)(Day09.answer1)
    assertResult(3444129546L)(Day09.answer2)

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
        |""".stripMargin)(Day10.sky.asString)
    assertResult(10813)(Day10.answer2)

  test("Day11"):
    assertResult("21,37")(Day11.answer1)
    assertResult("236,146,12")(Day11.answer2)

