package bowling

import scala.annotation.tailrec
import Frames.FrameNumber

object BowlingScore {
  def score(rollsStr: String): Either[String, Either[(Int, FrameNumber), Int]] =
    for {
      frames <- Frames(rollsStr)
      initScore <- initScore(frames)
    } yield if (frames.scorableSize == 10) Right(initScore + frames(10).tenthScore)
    else Left((initScore, frames.scorableSize))

  private def initScore(frames: Frames) = {
    @tailrec
    def loop(i: FrameNumber, accOpt: Either[String, Int]): Either[String, Int] =
      if (i == frames.scorableSize) accOpt
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