package bowling

import org.scalatest.FunSuite
import Frames.FrameLimitMsg
import Frame.PinLimitMsg

class BowlingScoreSuite extends FunSuite {
  def scoreTest(rollsStr: String, scoreOpt: Either[String, Int]) =
    assert(BowlingScore.score(rollsStr) === scoreOpt)

  test("perfect") {
    scoreTest("X X X X X X X X X XXX", Right(300))
  }
  test("9-") {
    scoreTest("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-", Right(90))
  }
  test("5/") {
    scoreTest("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5", Right(150))
  }
  test("almost perfect") {
    scoreTest("X X X X X X X X X X2/", Right(282))
  }
  test("almost perfect 2") {
    scoreTest("X X X X X X X X X -/X", Right(270))
  }
  test("miserable") {
    scoreTest("21 34 14 01 20 21 50 12 00 12", Right(32))
  }
  test("mix strike and regular") {
    scoreTest("X X X 45 X X X X X 81", Right(228))
  }
  test("frames-count-limit validation") {
    scoreTest("X X X 45 X X X X X 81 12", Left(FrameLimitMsg))
  }
  test("knocked-down-pin-count-limit validation") {
    scoreTest("X X X 47 X X X X X 81", Left(PinLimitMsg))
  }
}