package com.moonshot.model

import indigo._

final case class Ship(coords: Vector2, currentSpeed:Vector2, targetSpeed: Vector2) {
  val maxSpeed: Double = 150
  val acceleration: Double = 25;

  def update(gameTime: GameTime) = {
    val newSpeed = currentSpeed + ((targetSpeed - currentSpeed) * acceleration * gameTime.delta.value);
    val clampedSpeed = new Vector2(
      (
        if (newSpeed.x > maxSpeed)
          maxSpeed
        else if (newSpeed.x < -maxSpeed)
          -maxSpeed
        else
          newSpeed.x
      ),
      (
        if (newSpeed.y > maxSpeed)
          maxSpeed
        else if (newSpeed.y < -maxSpeed)
          -maxSpeed
        else
          newSpeed.y
      ),
    )

    this.copy(coords = coords + (clampedSpeed * gameTime.delta.value), currentSpeed = clampedSpeed)
  }

  def moveRight() =
    this.copy(targetSpeed = targetSpeed.withX(maxSpeed))

  def moveLeft() =
    this.copy(targetSpeed = targetSpeed.withX(-maxSpeed))

  def moveUp() =
    this.copy(targetSpeed = targetSpeed.withY(-maxSpeed))

  def moveDown() =
    this.copy(targetSpeed = targetSpeed.withY(maxSpeed))

  def stopHorizontal() =
    this.copy(targetSpeed = targetSpeed.withX(0))

  def stopVertical() =
    this.copy(targetSpeed = targetSpeed.withY(0))

  def toScreenSpace: Point =
    coords.toPoint
}

object Ship {
  val initial: Ship =
    Ship(Vector2.zero, Vector2.zero, Vector2.zero)
}
