package com.moonshot.scenes

import indigo._
import indigoextras.geometry.Bezier
import indigoextras.geometry.Vertex

/**
  * These are _not_ a good example of how to use Signals, but they have given me some ideas.
  */
object AnimationSignals {

  def fadeIn(over: Seconds): Signal[RGBA] =
    Signal { t =>
      RGBA.Black.withAmount(1 - (t / over).value)
    }.clampTime(Seconds(0), over)

  def crashShip(middle: Point, over: Seconds): Signal[Point] =
    Bezier(Vertex(middle.x.toDouble, -100), Vertex(middle.x.toDouble, 290))
      .toSignal(over)
      .map(_.toPoint)

  def textFlash(startAt: Seconds): Signal[Double] =
    Signal
      .product(
        Signal.Time,
        Signal
          .Pulse(Seconds(0.4))
          .map(p => if (p) 1.0 else 0.0)
      )
      .map {
        case (t, a) =>
          if (t > startAt) a else 0.0
      }

}
