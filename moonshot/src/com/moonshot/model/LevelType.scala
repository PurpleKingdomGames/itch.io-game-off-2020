package com.moonshot.model
import com.moonshot.model.LevelType.Lander
import com.moonshot.model.LevelType.Slalom
import com.moonshot.model.LevelType.Gauntlet

sealed trait LevelType {
  def renderLander: String
  def renderSlalom: String
  def renderGauntlet: String

  def up: LevelType =
    this match {
      case Lander   => Gauntlet
      case Slalom   => Lander
      case Gauntlet => Slalom
    }

  def down: LevelType =
    this match {
      case Lander   => Slalom
      case Slalom   => Gauntlet
      case Gauntlet => Lander
    }
}
object LevelType {
  case object Lander extends LevelType {
    val renderLander: String   = "[x] Fly me to the moon"
    val renderSlalom: String   = "[_] Lunar slalom"
    val renderGauntlet: String = "[_] Gibbous Gauntlet"
  }
  case object Slalom extends LevelType {
    val renderLander: String   = "[_] Fly me to the moon"
    val renderSlalom: String   = "[x] Lunar slalom"
    val renderGauntlet: String = "[_] Gibbous Gauntlet"
  }
  case object Gauntlet extends LevelType {
    val renderLander: String   = "[_] Fly me to the moon"
    val renderSlalom: String   = "[_] Lunar slalom"
    val renderGauntlet: String = "[x] Gibbous Gauntlet"
  }
}
