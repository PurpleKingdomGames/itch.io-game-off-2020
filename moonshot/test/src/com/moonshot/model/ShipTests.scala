package com.moonshot.model

import indigo._

import indigo.shared.time.GameTime.FPS
import indigoextras.geometry.LineSegment
import indigoextras.geometry.Vertex

class ShipTests extends munit.FunSuite {

  val gameTime: GameTime =
    GameTime.zero

  val ship: Ship =
    Ship
      .initial(Rectangle(0, 0, 500, 500))
      .rotateBy(Radians.TAUby2)
      .applyForce(Vector2(10, 10))

  val platform: LineSegment =
    LineSegment(Vertex(10, 1), Vertex(490, 1))

  // negative is above the line!
  test("no collision (below)") {

    val s: Ship =
      ship.moveTo(100, 100)

    val actual =
      Ship.updatePlatformCollisions(gameTime, List(platform))(s)

    val expected = s

    assertEquals(actual, expected)
    assertEquals(!platform.isFacingVertex(s.boundingBox.center), false)
  }

  test("no collision (above)") {

    val s: Ship =
      ship.moveTo(100, -100)

    val actual =
      Ship.updatePlatformCollisions(gameTime, List(platform))(s)

    val expected = s

    assertEquals(actual, expected)
    assertEquals(!platform.isFacingVertex(s.boundingBox.center), true)
  }

  test("landing") {

    val s: Ship =
      ship.moveTo(100, -1 - (ship.boundingBox.height / 2))

    val actual =
      Ship.updatePlatformCollisions(gameTime, List(platform))(s)

    val expected = s

    assertEquals(actual, expected)
  }

  test("crash landing") {

    val s: Ship =
      ship.moveTo(100, -5)

    val actual =
      Ship.updatePlatformCollisions(gameTime, List(platform))(s)

    val expected = s.copy(health = 0)

    assertEquals(actual, expected)
  }

  test("Real case") {
    val s =
      Ship(3, 3, Vector2(0, 0.1938), Vector2(320, -19.8062), Radians(0), Seconds(0), Seconds(0), 11.9702907)

    val ls = LineSegment(start = Vertex(-100, 1), end = Vertex(500, 1))

    assertEquals(Ship.fitToLand(s, ls), true)
  }

  test("Real case 2") {
    val s =
      Ship(3, 3, Vector2(0, 0.1938), Vector2(50, -2000), Radians(0), Seconds(0), Seconds(0), 11.9702907)

    val ls = LineSegment(start = Vertex(0, -2000), end = Vertex(100, -2000))

    assertEquals(Ship.fitToLand(s, ls), true)
  }

  def doubleCloseEnough(d1: Double, d2: Double): Boolean =
    d1 - 0.001 < d2 && d1 + 0.001 > d2

  test("wrap radians") {
    assertEquals(doubleCloseEnough(Ship.wrapRadians(Radians(0.0)).value, 0.0), true)
    assertEquals(doubleCloseEnough(Ship.wrapRadians(Radians(0.1)).value, 0.1), true)
    assertEquals(doubleCloseEnough(Ship.wrapRadians(Radians(-0.1)).value, Radians.TAU.value - 0.1), true)
    assertEquals(doubleCloseEnough(Ship.wrapRadians(Radians.TAU + Radians.TAUby4).value, Radians.TAUby4.value), true)
    assertEquals(doubleCloseEnough(Ship.wrapRadians(Radians.TAU - Radians.TAUby4).value, Radians.TAUby4.value * 3), true)
  }

  test("check yaw") {
    assertEquals(Ship.checkYaw(Radians(0)), true)
    assertEquals(Ship.checkYaw(Radians(-0.4)), true)
    assertEquals(Ship.checkYaw(Radians(0.4)), true)
    assertEquals(Ship.checkYaw(Radians(-4.0)), false)
    assertEquals(Ship.checkYaw(Radians(4.0)), false)
    assertEquals(Ship.checkYaw(Radians.TAUby2), false)
  }

  test("check speed limit") {
    assertEquals(Ship.withinSpeedLimit(Vector2(0, 0)), true)
    assertEquals(Ship.withinSpeedLimit(Vector2(2, 0)), true)
    assertEquals(Ship.withinSpeedLimit(Vector2(-2, 0)), true)
    assertEquals(Ship.withinSpeedLimit(Vector2(10, 0)), false)
    assertEquals(Ship.withinSpeedLimit(Vector2(-10, 0)), false)
    assertEquals(Ship.withinSpeedLimit(Vector2(0, 2)), true)
    assertEquals(Ship.withinSpeedLimit(Vector2(0, -2)), true)
    assertEquals(Ship.withinSpeedLimit(Vector2(0, 10)), false)
    assertEquals(Ship.withinSpeedLimit(Vector2(0, -10)), false)
  }

}
