package bowling

private[bowling] abstract class Frame(frameStr: String) {
  private[bowling] def isStrike: Boolean
  private[bowling] def isSpare: Boolean
  private[bowling] def simpleScore: Either[String, Int]
  private[bowling] def simpleScoreAt(i: Int): Int = Frame.simpleScore(frameStr.charAt(i))
  private[bowling] def tenthScore: Int
  private[bowling] def isComplete: Boolean
  private[Frame] def isSpare(i: Int) = frameStr.charAt(1) == '/'
  private[Frame] def simpleScoreAcc = frameStr.foldLeft(0)(_ + Frame.simpleScore(_))
}

private[bowling] object Frame {
  private[bowling] val PinLimitMsg = "Number of pins knocked down must not exceed 10 per frame"
  private[bowling] def apply(frameStr: String): Frame = new TwoFrame(frameStr)
  private[bowling] def tenth(frameStr: String): Frame = new TenthFrame(frameStr)
  private def simpleScore(c: Char) =
    c match {
      case '-'       => 0
      case 'X' | '/' => 10
      case c: Char   => c.asDigit
    }

  private class TwoFrame(frameStr: String) extends Frame(frameStr) {
    private[bowling] def isStrike = frameStr == "X"
    private[bowling] def isSpare = isSpare(1)
    private[bowling] def simpleScore =
      if (isSpare) Right(10)
      else {
        val ss = simpleScoreAcc
        if (ss > 10) Left(Frame.PinLimitMsg) else Right(ss)
      }
    private[bowling] def tenthScore = throw new NoSuchElementException("not 10th frame")
    private[bowling] def isComplete = frameStr.length == (if (isStrike) 1 else 2)
  }

  private class TenthFrame(frameStr: String) extends Frame(frameStr) {
    private[bowling] def isStrike = throw new NoSuchElementException("10th frame cannot be strike")
    private[bowling] def isSpare = isSpare(1) || isSpare(2)
    private[bowling] def simpleScore = throw new NoSuchElementException("not 1-9 frame")
    private[bowling] def tenthScore = if (isSpare) 10 + simpleScoreAt(2) else simpleScoreAcc
    private[bowling] def isComplete =
      frameStr.length > 1 && frameStr.length == (if (isStrike(0) || isSpare) 3 else 2)
    private def isStrike(i: Int) = frameStr.charAt(i) == 'X'
  }
}