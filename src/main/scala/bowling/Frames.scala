package bowling

import Frames.FrameNumber

private[bowling] trait Frames {
  private[bowling] def apply(i: FrameNumber): Frame
}

private[bowling] object Frames {
  private type FrameNumber = Int

  private[bowling] def apply(rollsStr: String): Either[String, Frames] = {
    val fs = new FramesImpl(rollsStr)
    if (fs.size > 10) Left("Number of frames must not exceed 10")
    else Right(fs)
  }

  private class FramesImpl(rollsStr: String) extends Frames {
    private val frames = rollsStr.split(' ')

    private[bowling] def apply(i: FrameNumber) = new Frame(frames(i - 1))
    private[bowling] def size = frames.size
  }
}