package com.moonshot.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex
import com.moonshot.model.ShipControl.Idle
import com.moonshot.model.ShipControl.TurnLeft
import com.moonshot.model.ShipControl.TurnRight
import com.moonshot.model.ShipControl.Thrust
import com.moonshot.model.ShipControl.ThrustLeft
import com.moonshot.model.ShipControl.ThrustRight

final case class Ship(health: Int, lives: Int, force: Vector2, coords: Vector2, angle: Radians) {
  val boundingBox: BoundingBox =
    new BoundingBox(new Vertex(coords.x - 16, coords.y - 24), new Vertex(32, 56));

  def update(gameTime: GameTime, asteroids: List[BoundingBox], shipControl: ShipControl, screenBounds: BoundingBox) =
    if (this.health < 1)
      this
        .updateMove(gameTime, ShipControl.TurnRight)
        .clampTo(screenBounds.copy(size = screenBounds.size.copy(y = screenBounds.size.y - 16)))
    else {
      val newShip = this
        .updateMove(gameTime, shipControl)
        .clampTo(screenBounds.copy(size = screenBounds.size.copy(y = screenBounds.size.y - 32)))
        .updateAsteroidCollisions(asteroids)

      if (newShip.health <= 0)
        newShip.copy(lives = newShip.lives - 1)
      else
        newShip
    }

  def moveBy(x: Double, y: Double) =
    this.copy(coords = this.coords + Vector2(x, y))

  def clampTo(clampBox: BoundingBox) =
    this.copy(coords =
      Vector2(
        Math.max(clampBox.x, Math.min(clampBox.x + clampBox.width - boundingBox.width, coords.x)),
        Math.max(clampBox.y, Math.min(clampBox.y + clampBox.height - boundingBox.height, coords.y))
      )
    )

  def toScreenSpace: Point =
    coords.toPoint

  def updateMove(gameTime: GameTime, shipControl: ShipControl): Ship = {
    val gravity             = 10.0d
    val windResistance      = Vector2(0.95, 0.95)
    val rotationSpeed       = Radians(7 * gameTime.delta.value)
    val angleReversed       = angle + Radians.TAUby2
    val acceleration        = 40 * gameTime.delta.value
    val gravityForce        = Vector2(0, Math.min(gravity, gravity * gameTime.delta.value))
    val nextForce           = (force + gravityForce) * windResistance
    val thrustForce         = Vector2(Math.sin(angleReversed.value) * acceleration, Math.cos(angleReversed.value) * acceleration)
    val nextForceWithThrust = (force + gravityForce + thrustForce) * windResistance

    shipControl match {
      case Idle =>
        this.copy(
          force = nextForce,
          coords = coords + nextForce
        )

      case TurnLeft =>
        this.copy(
          force = nextForce,
          coords = coords + nextForce,
          angle = angle + rotationSpeed
        )

      case TurnRight =>
        this.copy(
          force = nextForce,
          coords = coords + nextForce,
          angle = angle - rotationSpeed
        )

      case Thrust =>
        this.copy(
          force = nextForceWithThrust,
          coords = coords + nextForceWithThrust
        )

      case ThrustLeft =>
        this.copy(
          force = nextForceWithThrust,
          coords = coords + nextForceWithThrust,
          angle = angle + rotationSpeed
        )

      case ThrustRight =>
        this.copy(
          force = nextForceWithThrust,
          coords = coords + nextForceWithThrust,
          angle = angle - rotationSpeed
        )
    }
  }

  def updateAsteroidCollisions(asteroids: List[BoundingBox]) =
    asteroids.foldLeft(this) { (s, a) =>
      if (a.overlaps(s.boundingBox) && checkShipCollisionAgainstCircle(s.boundingBox, a))
        s.copy(health = s.health - 1)
      else
        s
    }

  def checkShipCollisionAgainstCircle(shipBox: BoundingBox, circleBox: BoundingBox) = {
    val center     = circleBox.center
    val halfShip   = shipBox.halfSize
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
  def initial(screenBounds: BoundingBox): Ship =
    Ship(1, 3, Vector2.zero, screenBounds.center.toVector2 + Vector2(0, 32), Radians.zero)

  val inputMappings: InputMapping[ShipControl] =
    InputMapping(
      Combo.KeyInputs(Key.LEFT_ARROW)                                -> ShipControl.TurnLeft,
      Combo.KeyInputs(Key.RIGHT_ARROW)                               -> ShipControl.TurnRight,
      Combo.KeyInputs(Key.UP_ARROW)                                  -> ShipControl.Thrust,
      Combo.KeyInputs(Key.LEFT_ARROW, Key.UP_ARROW)                  -> ShipControl.ThrustLeft,
      Combo.KeyInputs(Key.RIGHT_ARROW, Key.UP_ARROW)                 -> ShipControl.ThrustRight,
      Combo.KeyInputs(Key.LEFT_ARROW, Key.RIGHT_ARROW, Key.UP_ARROW) -> ShipControl.Thrust
    )

}

sealed trait ShipControl
object ShipControl {
  case object Idle        extends ShipControl
  case object TurnLeft    extends ShipControl
  case object TurnRight   extends ShipControl
  case object Thrust      extends ShipControl
  case object ThrustLeft  extends ShipControl
  case object ThrustRight extends ShipControl
}
