package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Asteroid(coords: Vector2, startRotation: Double, rotationSpeed: Double) {
  val boundingBox = BoundingBox(Vertex.fromVector(coords), Vertex(32, 32))

  def getBoundingBox() = boundingBox


  def moveBy(x: Double, y: Double) =
    this.copy(coords = this.coords + Vector2(x, y))
}

object Asteroid {

}