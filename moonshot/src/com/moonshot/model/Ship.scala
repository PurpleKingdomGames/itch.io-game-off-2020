package com.moonshot.model

import indigo._

final case class Ship(coords: Vector2) {
  val speed = 600

  def moveRight(gameTime: GameTime) =
    this.copy(coords = coords.withX((coords.x + (speed * gameTime.delta.value))))

  def moveLeft(gameTime: GameTime) =
    this.copy(coords = coords.withX((coords.x + -(speed * gameTime.delta.value))))

  def moveUp(gameTime: GameTime) =
    this.copy(coords = coords.withY((coords.y + -(speed * gameTime.delta.value))))

  def moveDown(gameTime: GameTime) =
    this.copy(coords = coords.withY((coords.y + (speed * gameTime.delta.value))))

  def toScreenSpace: Point =
    coords.toPoint
}

object Ship {
  val initial: Ship =
    Ship(Vector2.zero)
}
