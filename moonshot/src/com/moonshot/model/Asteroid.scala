package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Asteroid(coords: Vector2, orbit: Vector2, _type: AsteroidType, rotation: Radians, rotationSpeed: Radians) {
  val distance = coords.distanceTo(orbit)

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

  def update: Asteroid =
    this.copy(
      rotation = rotation + rotationSpeed,
      coords = Vector2(
        x = Math.sin(rotation.value) * distance + orbit.x,
        y = Math.cos(rotation.value) * distance + orbit.y
      )
    )

  def getBoundingBox = boundingBox

  def moveTo(x: Double, y: Double) =
    this.copy(coords = Vector2(x, y))

  def moveBy(x: Double, y: Double) =
    this.copy(coords = this.coords + Vector2(x, y))

  def withRotation(rotation: Radians) =
    this.copy(rotation = rotation)

  def withRotationSpeed(speed: Radians) =
    this.copy(rotationSpeed = speed)

  def withType(_type: AsteroidType) =
    this.copy(_type = _type)
}

sealed trait AsteroidType
object AsteroidType {
  case object Small       extends AsteroidType
  case object Medium      extends AsteroidType
  case object Big         extends AsteroidType
  case object ThatsNoMoon extends AsteroidType
}
