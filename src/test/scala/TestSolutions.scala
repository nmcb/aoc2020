import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(719796)(Day01.answer1)
    assertResult(144554112)(Day01.answer2)

  test("Day02"):
    assertResult(528)(Day02.answer1)
    assertResult(497)(Day02.answer2)

  test("Day03"):
    assertResult(242)(Day03.answer1)
    assertResult(2265549792L)(Day03.answer2)

  test("Day04"):
    assertResult(245)(Day04.answer1)
    assertResult(133)(Day04.answer2)

  test("Day05"):
    assertResult(806)(Day05.answer1)
    assertResult(562)(Day05.answer2)

  test("Day06"):
    assertResult(6680)(Day06.answer1)
    assertResult(3117)(Day06.answer2)

  test("Day07"):
    assertResult(208)(Day07.answer1)
    assertResult(1664)(Day07.answer2)

  test("Day08"):
    assertResult(1818)(Day08.answer1)
    assertResult(631)(Day08.answer2)

  test("Day09"):
    assertResult(373803594L)(Day09.answer1)
    assertResult(51152360L)(Day09.answer2)

  test("Day10"):
    assertResult(1820)(Day10.answer1)
    assertResult(3454189699072L)(Day10.answer2)
