package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Ship(health: Int, speed : Double, coords: Vector2, currentSpeed:Vector2, targetSpeed: Vector2) {
  val maxSpeed: Double = 300 * speed * 0.5
  val acceleration: Double = 25;
  val boundingBox: BoundingBox = new BoundingBox(new Vertex(coords.x, coords.y), new Vertex(32, 32));

  def update(gameTime: GameTime,  asteroids: List[BoundingBox]) = {
    if (this.health < 1)
      this
    else
      this
        .updateMove(gameTime)
        .updateAsteroidCollisions(asteroids)
  }

  def moveBy(x: Double, y: Double) =
    this.copy(coords = this.coords + Vector2(x, y))

  def clampTo(clampBox : BoundingBox) =
    this.copy(coords = Vector2(
      Math.max(clampBox.x, Math.min(clampBox.x + clampBox.width - boundingBox.width, coords.x)),
      Math.max(clampBox.y, Math.min(clampBox.y + clampBox.height - boundingBox.height, coords.y))
    ))

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

  def updateMove(gameTime: GameTime) = {
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

  def updateAsteroidCollisions(asteroids: List[BoundingBox]) = {
    asteroids.foldLeft(this)((s, a) => {
      if (a.overlaps(s.boundingBox) && checkShipCollisionAgainstCircle(s.boundingBox, a))
        s.copy(health = s.health - 1)
      else
        s
    })
  }

  def checkShipCollisionAgainstCircle(shipBox: BoundingBox, circleBox: BoundingBox) = {
    val center = circleBox.center
    val halfShip = shipBox.halfSize
    val shipCenter = shipBox.center

    val diff = center - shipCenter
    val clamped = Vertex(
        Math.max(-halfShip.x, Math.min(halfShip.x, diff.x)),
        Math.max(-halfShip.y, Math.min(halfShip.y, diff.y))
    )

    val closest = shipCenter + clamped
    closest.distanceTo(center) < (circleBox.width * 0.5)
  }
}

object Ship {
  val initial: Ship =
    Ship(1, 1, Vector2(159, 625), Vector2.zero, Vector2.zero)
}
