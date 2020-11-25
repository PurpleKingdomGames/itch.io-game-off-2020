package com.moonshot.model

import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Radians

final case class Camera(target: Point) {
  def update(shipCoords: Point, shipRotation: Radians, maxHeight: Int, screenHeight: Int): Camera = {
    val gridHeight    = 32
    val distanceAhead = 75

    val targetY: Int =
      Math.min(
        -distanceAhead - gridHeight,
        Math.max(
          shipCoords.y - (Math.cos(shipRotation.value) * distanceAhead).toInt,
          -maxHeight + (screenHeight / 2) - gridHeight
        )
      )

    this.copy(
      target = Point(
        x = 0,
        y = targetY
      )
    )
  }

  def inverse: Point =
    Point(-target.x, -target.y)
}
object Camera {
  def initial: Camera =
    Camera(Point.zero)
}
