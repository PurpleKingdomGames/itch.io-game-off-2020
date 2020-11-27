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
import indigoextras.geometry.LineSegment

final case class Ship(health: Int, lives: Int, force: Vector2, coords: Vector2, angle: Radians, lastImpact: Seconds, lastDeath: Seconds, gravity: Double) {
  val bounds: BoundingBox =
    BoundingBox(Vertex(0, 0), Vertex(32, 64))
  val boundingBox: BoundingBox =
    bounds.moveTo(Vertex(coords.x - (bounds.width / 2), coords.y - (bounds.height / 2)))

  def update(gameTime: GameTime, asteroids: List[BoundingBox], platforms: List[LineSegment], shipControl: ShipControl, screenBounds: BoundingBox, courseHeight: Int): Ship =
    if (health < 1)
      Ship.updateMove(gameTime, ShipControl.TurnRight)(this)
    else {
      val newShip =
        (Ship.updateMove(gameTime, shipControl) _ andThen
          Ship.applyGravity(courseHeight) andThen
          Ship.clampTo(screenBounds, courseHeight) andThen
          Ship.updateAsteroidCollisions(gameTime, asteroids) andThen
          Ship.updatePlatformCollisions(gameTime, platforms))(this)

      if (newShip.health <= 0)
        newShip.copy(
          lives = newShip.lives - 1,
          lastDeath = gameTime.running
        )
      else
        newShip
    }

  def moveTo(position: Vector2): Ship =
    this.copy(coords = position)
  def moveTo(x: Double, y: Double): Ship =
    moveTo(Vector2(x, y))

  def moveBy(amount: Vector2): Ship =
    moveTo(this.coords + amount)
  def moveBy(x: Double, y: Double): Ship =
    moveBy(Vector2(x, y))

  def applyForce(newForce: Vector2): Ship =
    this.copy(force = force * newForce)

  def rotateTo(newAngle: Radians): Ship =
    this.copy(angle = newAngle)

  def rotateBy(amount: Radians): Ship =
    this.copy(angle = angle + amount)

  def toScreenSpace: Point =
    coords.toPoint

}

object Ship {

  val StandardGravity: Double = 12.0d

  val invulnerableFor: Seconds =
    Seconds(1.5)

  def initial(screenBounds: Rectangle): Ship =
    Ship(
      3,
      3,
      Vector2.zero,
      Vector2(screenBounds.center.x.toDouble, -20),
      Radians.zero,
      Seconds.zero,
      Seconds.zero,
      StandardGravity
    )

  val inputMappings: InputMapping[ShipControl] =
    InputMapping(
      Combo.KeyInputs(Key.LEFT_ARROW)                                -> ShipControl.TurnLeft,
      Combo.KeyInputs(Key.RIGHT_ARROW)                               -> ShipControl.TurnRight,
      Combo.KeyInputs(Key.UP_ARROW)                                  -> ShipControl.Thrust,
      Combo.KeyInputs(Key.LEFT_ARROW, Key.UP_ARROW)                  -> ShipControl.ThrustLeft,
      Combo.KeyInputs(Key.RIGHT_ARROW, Key.UP_ARROW)                 -> ShipControl.ThrustRight,
      Combo.KeyInputs(Key.LEFT_ARROW, Key.RIGHT_ARROW, Key.UP_ARROW) -> ShipControl.Thrust
    )

  def clampTo(clampBox: BoundingBox, courseHeight: Int)(ship: Ship): Ship =
    ship.copy(coords =
      Vector2(
        x = Math.max(clampBox.left + 10, Math.min(ship.coords.x, clampBox.right - 10)),
        y = Math.max(-courseHeight.toDouble, Math.min(ship.coords.y, 0))
      )
    )

  def updateMove(gameTime: GameTime, shipControl: ShipControl)(ship: Ship): Ship = {
    val windResistance      = Vector2(0.95, 0.95)
    val rotationSpeed       = Radians(5 * gameTime.delta.value)
    val angleReversed       = ship.angle + Radians.TAUby2
    val acceleration        = 40 * gameTime.delta.value
    val gravityForce        = Vector2(0, Math.min(ship.gravity, ship.gravity * gameTime.delta.value))
    val nextForce           = (ship.force + gravityForce) * windResistance
    val thrustForce         = Vector2(Math.sin(angleReversed.value) * acceleration, Math.cos(angleReversed.value) * acceleration)
    val nextForceWithThrust = (ship.force + gravityForce + thrustForce) * windResistance

    shipControl match {
      case Idle =>
        ship.copy(
          force = nextForce,
          coords = ship.coords + nextForce
        )

      case TurnLeft =>
        ship.copy(
          force = nextForce,
          coords = ship.coords + nextForce,
          angle = ship.angle + rotationSpeed
        )

      case TurnRight =>
        ship.copy(
          force = nextForce,
          coords = ship.coords + nextForce,
          angle = ship.angle - rotationSpeed
        )

      case Thrust =>
        ship.copy(
          force = nextForceWithThrust,
          coords = ship.coords + nextForceWithThrust
        )

      case ThrustLeft =>
        ship.copy(
          force = nextForceWithThrust,
          coords = ship.coords + nextForceWithThrust,
          angle = ship.angle + rotationSpeed
        )

      case ThrustRight =>
        ship.copy(
          force = nextForceWithThrust,
          coords = ship.coords + nextForceWithThrust,
          angle = ship.angle - rotationSpeed
        )
    }
  }

  def updateAsteroidCollisions(gameTime: GameTime, asteroids: List[BoundingBox])(ship: Ship): Ship =
    asteroids.foldLeft(ship) { (s, a) =>
      if (gameTime.running - ship.lastImpact > Ship.invulnerableFor && a.overlaps(s.boundingBox) && checkShipCollisionAgainstCircle(s.boundingBox, a))
        s.copy(
          health = Math.max(0, s.health - 1),
          lastImpact = gameTime.running,
          force = Vector2(-ship.force.x * 1.5, -ship.force.y * 1.5)
        )
      else
        s
    }

  def checkShipCollisionAgainstCircle(shipBox: BoundingBox, circleBox: BoundingBox): Boolean = {
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

  def updatePlatformCollisions(gameTime: GameTime, platforms: List[LineSegment])(ship: Ship): Ship =
    platforms
      .map(p => ship.boundingBox.lineIntersectsAt(p).map(_ => p))
      .collect { case Some(s) => Some(s) }
      .headOption
      .flatten match {
      case Some(ls) if fitToLand(ship, ls) =>
        ship.copy(
          force = Vector2.zero,
          angle = Radians(0),
          coords = ship.coords.withY(ls.start.y - (ship.bounds.height / 2))
        )

      case Some(_) =>
        ship.copy(
          health = 0,
          lastImpact = gameTime.running,
          force = Vector2(-ship.force.x * 1.5, -ship.force.y * 1.5)
        )

      case None =>
        ship
    }

  def applyGravity(courseHeight: Int)(ship: Ship): Ship =
    ship.copy(
      gravity = Ship.StandardGravity * (1 - -(ship.coords.y / courseHeight.toDouble))
    )

  def wrapRadians(r: Radians): Radians =
    Radians(((r.value % Radians.TAU.value) + Radians.TAU.value) % Radians.TAU.value)

  def fitToLand(ship: Ship, lineSegment: LineSegment): Boolean =
    !lineSegment.isFacingVertex(ship.boundingBox.center) && // ship is above
      checkYaw(ship.angle) &&                               // Angle close to vertical
      withinSpeedLimit(ship.force)

  def checkYaw(angle: Radians): Boolean = {
    val yawAllowed: Double = 1.0
    val a                  = wrapRadians(angle + Radians(yawAllowed / 2)).value

    a >= 0 && a <= yawAllowed
  }

  def withinSpeedLimit(force: Vector2): Boolean = {
    val xLimit = 5
    val yLimit = 5

    (force.x >= -xLimit && force.x <= xLimit && force.y >= -yLimit && force.y <= yLimit)
  }
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
