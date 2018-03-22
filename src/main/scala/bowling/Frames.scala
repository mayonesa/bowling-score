package bowling

import Frames.FrameNumber

private[bowling] trait Frames {
  private[bowling] def apply(i: FrameNumber): Frame
  private[bowling] def size: Int
}

private[bowling] object Frames {
  private[bowling] val FrameLimitMsg = "Number of frames must not exceed 10"
  private[bowling] type FrameNumber = Int

  private[bowling] def apply(rollsStr: String): Either[String, Frames] = {
    val fs = new FramesImpl(rollsStr)
    if (fs.size > 10) Left(FrameLimitMsg)
    else Right(fs)
  }

  private class FramesImpl(rollsStr: String) extends Frames {
    private val frames = rollsStr.split(' ')

    private[bowling] def apply(i: FrameNumber) = Frame(frames(i - 1))
    private[bowling] def size = {
      val lastFrame = frame(frames.size)
      frames.size + (if (lastFrame.isComplete)
        if (frames.size < 10) {
        if (lastFrame.isStrike)
          if (frames.size > 1) {
            val penultimateFrame = frame(frames.size - 1)
            if (penultimateFrame.isStrike)
              if (frames.size > 2) {
                val antePenultimateFrame = frame(frames.size - 2)
                if (antePenultimateFrame.isStrike) -2 else -1
              } else -1
            else 0
          } else 0
        else if (lastFrame.isSpare) -1
        else 0
      } else 1
      else 0)
    }
    private def frame(i: FrameNumber) = Frame(frames(i - 1))
  }
}