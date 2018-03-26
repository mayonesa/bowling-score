package bowling

import org.scalatest.FunSuite
import bowling.Frames.FrameNumber

class FramesSuite extends FunSuite {
  def sizeTest(rollsStr: String, size: FrameNumber) =
    Frames(rollsStr) match {
      case Right(fs) => assert(fs.scorableSize === size)
      case Left(msg) => fail(msg)
    }

  test("full") {
    sizeTest("X X X X X X X X X XXX", 10)
  }
  test("incomplete 10 (XX)") {
    sizeTest("X X X X X X X X X XX", 9)
  }
  test("incomplete 10 (X)") {
    sizeTest("X X X X X X X X X X", 8)
  }
}