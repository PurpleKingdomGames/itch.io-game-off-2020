package com.moonshot.model

import indigo._

final case class Upgrade(speedBoost: Double, healthBoost: Int, cost: Seconds) {}

object Upgrade {
  val armour =
    Upgrade(0, 1, Seconds(0.5))

  val thrusters =
    Upgrade(2, 0, Seconds(1))
}
