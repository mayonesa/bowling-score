package bowling

import org.scalatest.FunSuite

class BowlingScoreSuite extends FunSuite {
  def scoreTest(rollsStr: String, score: Int) = assert(BowlingScore.score(rollsStr) === score)
	
  test("perfect") {
		scoreTest("X X X X X X X X X XXX", 300)
	}
	test("9-") {
		scoreTest("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-", 90)
	}
	test("5/") {
		scoreTest("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5", 150)
	}
  test("almost perfect") {
		scoreTest("X X X X X X X X X X2/", 282)
	}
  test("almost perfect 2") {
		scoreTest("X X X X X X X X X -/X", 270)
	}
  test("miserable") {
		scoreTest("21 34 14 01 20 21 50 12 00 12", 32)
	}
  test("mix strike and regular") {
		scoreTest("X X X 45 X X X X X 81", 228)
	}
}