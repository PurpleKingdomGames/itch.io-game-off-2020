package com.moonshot.model

import indigo._

import utest._
import indigo.shared.time.GameTime.FPS

object ShipTests extends TestSuite {

  val tests: Tests =
    Tests {

      "Ship movement" - {

        "should move ship to the right after a full second" - {
          val ship     = Ship(Vector2(10, 10))
          val gameTime = GameTime(Seconds(1), Seconds(1), FPS.`60`)

          val expected = Vector2(15, 10)

          val actual = ship.moveRight(gameTime).coords

          actual ==> expected
        }

        "should move ship to the right after a single frame" - {
          val ship     = Ship(Vector2(10, 10))
          val gameTime = GameTime(Seconds(1), Seconds(0.01666666667), FPS.`60`)

          val expected = Vector2(10.08333333333, 10)

          val actual = ship.moveRight(gameTime).coords

          actual ==> expected
        }

      }

    }

}
