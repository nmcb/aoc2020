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
