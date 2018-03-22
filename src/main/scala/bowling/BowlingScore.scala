package bowling

import Frames.FrameNumber

object BowlingScore {
  def score(rollsStr: String): Either[String, Int] =
    for {
      frames <- Frames(rollsStr)
      scoreOpt <- score(frames)
    } yield scoreOpt

  private def score(frames: Frames) = {
    def loop(i: FrameNumber, accOpt: Either[String, Int]): Either[String, Int] =
      if (i == 10) accOpt.map(_ + frames(10).tenthScore)
      else {
        val frame = frames(i)
        lazy val nextFrame = frames(i + 1)
        def secondBonusTerm =
          if (nextFrame.isStrike) frames(i + 2).simpleScoreAt(0)
          else nextFrame.simpleScoreAt(1)
        def bonus = nextFrame.simpleScoreAt(0) + (if (frame.isStrike) secondBonusTerm else 0)
        frame.simpleScore match {
          case Right(simpleScore) =>
            loop(i + 1, accOpt.map {
              _ + simpleScore + (if (frame.isStrike || frame.isSpare) bonus else 0)
            })
          case left => left
        }
      }

    loop(1, Right(0))
  }
}