package bowling

object BowlingScore {
  def score(rollsStr: String): Int = {
    val frames = Frames(rollsStr)
    (1 to 9).foldLeft(0) { (acc, i) =>
      val frame = frames(i)
      def nextFrame = frames(i + 1)
      def secondBonusTerm =
        if (nextFrame.isStrike) frames(i + 2).simpleScoreAt(0)
        else nextFrame.simpleScoreAt(1)
      def bonus = nextFrame.simpleScoreAt(0) + (if (frame.isStrike) secondBonusTerm else 0)
      acc + frame.simpleScore + (if (frame.isStrike || frame.isSpare) bonus else 0)
    } + frames(10).tenthScore
  }
}