package com.moonshot.model

final case class Upgrade(speedBoost: Double, healthBoost: Int, cost: Double) {}

object Upgrade {
  val armour =
    Upgrade(0, 1, 0.5)

  val thrusters =
    Upgrade(2, 0, 1)
}
