package com.moonshot.model

import indigo._

import utest._
import indigo.shared.time.GameTime.FPS
import indigoextras.geometry.LineSegment
import indigoextras.geometry.Vertex

object ShipTests extends TestSuite {

  val tests: Tests =
    Tests {

      "Ship" - {

        val ship: Ship =
          Ship
            .initial(Rectangle(0, 0, 500, 500))
            .rotateBy(Radians.TAUby2)
            .applyForce(Vector2(10, 10))

        "platform collisions" - {

          val platform: LineSegment =
            LineSegment(Vertex(10, 1), Vertex(490, 1))

          // negative is above the line!
          "no collision (below)" - {

            val s: Ship =
              ship.moveTo(100, 100)

            val actual =
              Ship.updatePlatformCollisions(List(platform))(s)

            val expected = s

            actual ==> expected
            !platform.isFacingVertex(s.boundingBox.center) ==> false
          }

          "no collision (above)" - {

            val s: Ship =
              ship.moveTo(100, -100)

            val actual =
              Ship.updatePlatformCollisions(List(platform))(s)

            val expected = s

            actual ==> expected
            !platform.isFacingVertex(s.boundingBox.center) ==> true
          }

          "landing" - {

            val s: Ship =
              ship.moveTo(100, -1 - (ship.boundingBox.height / 2))

            val actual =
              Ship.updatePlatformCollisions(List(platform))(s)

            val expected = s

            actual ==> expected
          }

          "crash landing" - {

            val s: Ship =
              ship.moveTo(100, -5)

            val actual =
              Ship.updatePlatformCollisions(List(platform))(s)

            val expected = s.copy(health = 0)

            actual ==> expected
          }

          "Real case" - {
            val s =
              Ship(3, 3, Vector2(0, 0.1938), Vector2(320, -19.8062), Radians(0), Seconds(0), Seconds(0), 11.9702907)

            val ls = LineSegment(start = Vertex(-100, 1), end = Vertex(500, 1))

            Ship.fitToLand(s, ls) ==> true
          }

          "Real case 2" - {
            val s =
              Ship(3, 3, Vector2(0, 0.1938), Vector2(50, -2000), Radians(0), Seconds(0), Seconds(0), 11.9702907)

            val ls = LineSegment(start = Vertex(0, -2000), end = Vertex(100, -2000))

            Ship.fitToLand(s, ls) ==> true
          }

        }

      }

      "Utils" - {

        def doubleCloseEnough(d1: Double, d2: Double): Boolean =
          d1 - 0.001 < d2 && d1 + 0.001 > d2

        "wrap radians" - {
          doubleCloseEnough(Ship.wrapRadians(Radians(0.0)).value, 0.0) ==> true
          doubleCloseEnough(Ship.wrapRadians(Radians(0.1)).value, 0.1) ==> true
          doubleCloseEnough(Ship.wrapRadians(Radians(-0.1)).value, Radians.TAU.value - 0.1) ==> true
          doubleCloseEnough(Ship.wrapRadians(Radians.TAU + Radians.TAUby4).value, Radians.TAUby4.value) ==> true
          doubleCloseEnough(Ship.wrapRadians(Radians.TAU - Radians.TAUby4).value, Radians.TAUby4.value * 3) ==> true
        }

        "check yaw" - {
          Ship.checkYaw(Radians(0)) ==> true
          Ship.checkYaw(Radians(-0.4)) ==> true
          Ship.checkYaw(Radians(0.4)) ==> true
          Ship.checkYaw(Radians(-4.0)) ==> false
          Ship.checkYaw(Radians(4.0)) ==> false
          Ship.checkYaw(Radians.TAUby2) ==> false
        }

        "check speed limit" - {
          Ship.withinSpeedLimit(Vector2(0, 0)) ==> true
          Ship.withinSpeedLimit(Vector2(2, 0)) ==> true
          Ship.withinSpeedLimit(Vector2(-2, 0)) ==> true
          Ship.withinSpeedLimit(Vector2(10, 0)) ==> false
          Ship.withinSpeedLimit(Vector2(-10, 0)) ==> false
          Ship.withinSpeedLimit(Vector2(0, 2)) ==> true
          Ship.withinSpeedLimit(Vector2(0, -2)) ==> true
          Ship.withinSpeedLimit(Vector2(0, 10)) ==> false
          Ship.withinSpeedLimit(Vector2(0, -10)) ==> false
        }

      }

    }

}
