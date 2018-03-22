package bowling

import Frames.FrameNumber

private[bowling] class Frames(rollsStr: String) {
  private val frames = rollsStr.split(' ')
  private[bowling] def apply(i: FrameNumber) = new Frame(frames(i - 1))
}

private[bowling] object Frames {
  private type FrameNumber = Int
  private[bowling] def apply(rollsStr: String) = new Frames(rollsStr)
}