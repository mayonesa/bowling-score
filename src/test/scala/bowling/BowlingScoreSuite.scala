package bowling

import org.scalatest.FunSuite
import Frames.FrameLimitMsg
import Frame.PinLimitMsg
import Frames.FrameNumber

class BowlingScoreSuite extends FunSuite {
  def scoreTest(rollsStr: String, scoreOpt: Either[String, Either[(Int, FrameNumber), Int]]) =
    assert(BowlingScore.score(rollsStr) === scoreOpt)

  test("perfect") {
    scoreTest("X X X X X X X X X XXX", Right(Right(300)))
  }
  test("9-") {
    scoreTest("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-", Right(Right(90)))
  }
  test("5/") {
    scoreTest("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5", Right(Right(150)))
  }
  test("almost perfect") {
    scoreTest("X X X X X X X X X X2/", Right(Right(282)))
  }
  test("almost perfect 2") {
    scoreTest("X X X X X X X X X -/X", Right(Right(270)))
  }
  test("miserable") {
    scoreTest("21 34 14 01 20 21 50 12 00 12", Right(Right(32)))
  }
  test("mix strike and regular") {
    scoreTest("X X X 45 X X X X X 81", Right(Right(228)))
  }
  test("frames-count-limit validation") {
    scoreTest("X X X 45 X X X X X 81 12", Left(FrameLimitMsg))
  }
  test("knocked-down-pin-count-limit validation") {
    scoreTest("X X X 47 X X X X X 81", Left(PinLimitMsg))
  }
  test("frames-count-limit validation even when knocked-down-pin-count-limit violated") {
    scoreTest("X X X 47 X X X X X 81 12", Left(FrameLimitMsg))
  }
  test("partial-frames scoring") {
    scoreTest("X X X 45 X X X X X", Right(Left(227, 9)))
  }
  test("partial-10th-frame scoring") {
    scoreTest("X X X 45 X X X X X 8", Right(Left(227, 9)))
  }
  test("partial-frame scoring") {
    scoreTest("X X X 45 X X X X 8", Right(Left(227, 9)))
  }
}