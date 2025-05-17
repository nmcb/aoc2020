import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(719796)(Day01.answer1)
    assertResult(144554112)(Day01.answer2)

  test("Day02"):
    assertResult(528)(Day02.answer1)
    assertResult(497)(Day02.answer2)
