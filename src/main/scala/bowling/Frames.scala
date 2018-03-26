package bowling

import Frames.FrameNumber

private[bowling] trait Frames {
  private[bowling] def apply(i: FrameNumber): Frame
  private[bowling] def scorableSize: Int
}

private[bowling] object Frames {
  private[bowling] val FrameLimitMsg = "Number of frames must not exceed 10"
  private[bowling] type FrameNumber = Int

  private[bowling] def apply(rollsStr: String): Either[String, Frames] = {
    val fs = new FramesImpl(rollsStr)
    if (fs.scorableSize > 10) Left(FrameLimitMsg)
    else Right(fs)
  }

  private class FramesImpl(rollsStr: String) extends Frames {
    private val framesArr = rollsStr.split(' ')

    private[bowling] def apply(i: FrameNumber) = Frame(framesArr(i - 1))
    private[bowling] def scorableSize = {
      val lastFrame = frame(framesArr.size)
      framesArr.size + (if (lastFrame.isComplete) 0 else -1) +
        (if (framesArr.size < 10) {
          if (lastFrame.isStrike)
            if (framesArr.size > 1) {
              val penultimateFrame = frame(framesArr.size - 1)
              if (penultimateFrame.isStrike)
                if (framesArr.size > 2) {
                  val antePenultimateFrame = frame(framesArr.size - 2)
                  if (antePenultimateFrame.isStrike) -2 else -1
                } else -1 // size <= 2
              else 0 // penultimate !strike
            } else 0 // size <= 1
          else if (lastFrame.isSpare) -1
          else 0 // last frame !spare and !strike
        } else { // ten frames
          val penultimateFrame = frame(framesArr.size - 1)
          if (penultimateFrame.isStrike) {
            val antePenultimateFrame = frame(framesArr.size - 2)
            if (antePenultimateFrame.isStrike) -1 else 0
          } else 0 // prenultimate !strike
        })
    }
    private def frame(i: FrameNumber) = Frame(framesArr(i - 1), i == 10)
  }
}