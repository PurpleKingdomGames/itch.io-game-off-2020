package com.moonshot.model

import indigo.Point
import indigo.shared.time.GameTime

final case class Ship (coords: Point) {
    val speed = 5

    def moveRight(gameTime: GameTime) =
        this.copy(coords = coords.withX((coords.x + (speed * gameTime.delta.value)).toInt))

    def moveLeft(gameTime: GameTime) =
        this.copy(coords = coords.withX((coords.x + -(speed * gameTime.delta.value)).toInt))

    def moveUp(gameTime: GameTime) =
        this.copy(coords = coords.withY((coords.y + -(speed * gameTime.delta.value)).toInt))

    def moveDown(gameTime: GameTime) =
        this.copy(coords = coords.withY((coords.y + (speed * gameTime.delta.value)).toInt))
}


object Ship {
  val initial: Ship =
    Ship(new Point(0, 0))
}