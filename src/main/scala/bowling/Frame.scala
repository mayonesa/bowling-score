package bowling

private[bowling] object Frame {
  private[bowling] val PinLimitMsg = "Number of pins knocked down must not exceed 10 per frame"
}

private[bowling] class Frame(frameStr: String) {
  private[bowling] def isStrike = frameStr == "X"
  private[bowling] def isSpare: Boolean = isSpare(1) || isSpare(2)
  private[bowling] def simpleScoreAt(i: Int) = simpleScore(frameStr.charAt(i))
  private[bowling] def simpleScore: Either[String, Int] =
    if (isSpare) Right(10)
    else {
      val ss = simpleScoreAcc
      if (ss > 10) Left(Frame.PinLimitMsg) else Right(ss)
    }
  private[bowling] def tenthScore: Int = if (isSpare) 10 + simpleScoreAt(2) else simpleScoreAcc
  private def simpleScore(c: Char): Int =
    c match {
      case '-'       => 0
      case 'X' | '/' => 10
      case c: Char   => c.asDigit
    }
  private def simpleScoreAcc = frameStr.foldLeft(0)(_ + simpleScore(_))
  private def isSpare(i: Int): Boolean = frameStr.length > i && frameStr.charAt(i) == '/'
}