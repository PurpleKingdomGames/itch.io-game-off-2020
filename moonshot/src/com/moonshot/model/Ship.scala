package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex

final case class Ship(health: Int, coords: Vector2, currentSpeed:Vector2, targetSpeed: Vector2) {
  val maxSpeed: Double = 150
  val acceleration: Double = 25;
  val boundingBox: BoundingBox = new BoundingBox(new Vertex(coords.x, coords.y), new Vertex(32, 32));

  def update(gameTime: GameTime, asteroids: List[BoundingBox]) = {
    if (this.health < 1)
      this
    else
      this
        .updateMove(gameTime)
        .updateAsteroidCollisions(asteroids)
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
    val halfShip = shipBox.halfSize + shipBox.center
    val centreVector = Vector2.fromPoints(circleBox.center.toPoint, shipBox.center.toPoint);
    val clampedVertex = new Vertex(
      Math.max(-halfShip.x, Math.min(halfShip.x, centreVector.x)),
      Math.max(-halfShip.y, Math.min(halfShip.y, centreVector.y))
    )

    val closest = shipBox.center + clampedVertex
    Vertex.distanceBetween(closest, circleBox.center) < circleBox.halfSize.x
  }
}

object Ship {
  val initial: Ship =
    Ship(1, Vector2(175, 625), Vector2.zero, Vector2.zero)
}
