package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Asteroid(coords: Vector2, _type: AsteroidType, startRotation: Double, rotationSpeed: Double) {
  val boundingBox = {
    val vertex = _type match {
      case AsteroidType.Small =>
        Vertex(14, 14)
      case AsteroidType.Medium =>
        Vertex(28, 28)
      case AsteroidType.Big =>
        Vertex(58, 58)
      case AsteroidType.ThatsNoMoon =>
        Vertex(124, 124)
    }

    BoundingBox(Vertex.fromVector(coords) - (vertex * 0.5), vertex)
  }

  def getBoundingBox = boundingBox

  def moveTo(x: Double, y: Double) =
    this.copy(coords = Vector2(x, y))

  def moveBy(x: Double, y: Double) =
    this.copy(coords = this.coords + Vector2(x, y))

  def withRotation(rotation: Double) =
    this.copy(startRotation = rotation)

  def withRotationSpeed(speed: Double) =
    this.copy(rotationSpeed = speed)

  def withType(_type: AsteroidType) =
    this.copy(_type = _type)
}

object Asteroid {
  val initial =
    Asteroid(Vector2.zero, AsteroidType.Small, 0, 0)
}

sealed trait AsteroidType
object AsteroidType {
  case object Small       extends AsteroidType
  case object Medium      extends AsteroidType
  case object Big         extends AsteroidType
  case object ThatsNoMoon extends AsteroidType
}
