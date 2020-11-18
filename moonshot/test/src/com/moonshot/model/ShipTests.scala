package com.moonshot.model

import indigo._

import utest._
import indigo.shared.time.GameTime.FPS

object ShipTests extends TestSuite {

  val tests: Tests =
    Tests {

      "Ship movement" - {

        "should move ship to the right after a full second" - {
          val ship     = Ship(1, 1, Vector2(10, 10), Vector2.zero, Vector2.zero)
          val gameTime = GameTime(Seconds(1), Seconds(1), FPS.`60`)

          val expected = Vector2(160, 10)

          val actual = ship.moveRight().update(gameTime, Nil).coords

          actual ==> expected
        }

        "should move ship to the right after a single frame" - {
          val ship     = Ship(1, 1, Vector2(10, 10), Vector2.zero, Vector2.zero)
          val gameTime = GameTime(Seconds(1), Seconds(0.01666666667), FPS.`60`)

          val actual = ship.moveRight().update(gameTime, Nil).coords

          actual.x > ship.coords.x ==> true
          (actual.x > 11.0 && actual.x < 12.0) ==> true
        }

      }

    }

}
