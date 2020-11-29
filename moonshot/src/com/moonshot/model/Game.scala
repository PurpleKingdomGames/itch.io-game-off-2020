package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox
import com.moonshot.viewmodel.ScreenBoundsUpdated
import com.moonshot.model.Course
import com.moonshot.model.Belt
import com.moonshot.model.Camera

final case class Game(
    levelType: LevelType,
    gameState: GameState,
    ship: Ship,
    timeRemainingInSeconds: Seconds,
    asteroids: List[Asteroid],
    // initialSpeed: Double,
    // targetVerticalSpeed: Double,
    // verticalOffset: Double,
    // nextAsteroidSpawn: Double,
    // targetVerticalOffset: Double,
    screenBounds: Rectangle,
    course: Course,
    camera: Camera,
    debugMode: Boolean
) {
  val maxAsteroids: Int                  = 20
  val initMinAsteroidSpawnRate: Double   = 1
  val initMaxAsteroidSpawnRate: Double   = 0.5
  val targetMinAsteroidSpawnRate: Double = 0.1
  val targetMaxAsteroidSpawnRate: Double = 0.25

  def update(gameTime: GameTime, dice: Dice, shipControl: ShipControl): GlobalEvent => Outcome[Game] = {
    case ScreenBoundsUpdated(newScreenBounds) =>
      Outcome(this.copy(screenBounds = newScreenBounds))

    case FrameTick =>
      gameState match {
        case GameState.GameRunning if ship.hasLandedOnMoon =>
          Outcome(this.copy(gameState = GameState.GameWin))

        case GameState.GameRunning if timeRemainingInSeconds.toDouble <= 0 =>
          Outcome(this.copy(gameState = GameState.GameLoss))

        case GameState.GameRunning =>
          updateRunningGame(gameTime /*, dice*/, shipControl)

        case _ =>
          Outcome(this)
      }

    case KeyUp(Key.ESCAPE) =>
      gameState match {
        case GameState.GameRunning if ship.lastDeath == Seconds.zero =>
          Outcome(this.copy(gameState = GameState.GamePaused))

        case GameState.GamePaused =>
          Outcome(this.copy(gameState = GameState.GameRunning))

        case _ =>
          Outcome(this)
      }

    case KeyUp(Key.ENTER) =>
      gameState match {
        case GameState.GameWin | GameState.GameLoss =>
          Outcome(Game.initial(dice, screenBounds))

        case _ =>
          Outcome(this)
      }

    case _ =>
      Outcome(this)
  }

  // def buyUpgrade(upgrade: Upgrade) =
  //   if (timeRemainingInSeconds > upgrade.cost)
  //     this.copy(
  //       ship = ship.copy(health = ship.health + upgrade.healthBoost),
  //       timeRemainingInSeconds =
  //         timeRemainingInSeconds - upgrade.cost,
  //       initialSpeed = initialSpeed + upgrade.speedBoost
  //     )
  //   else
  //     this

  // def spawnAsteroid(gameTime: GameTime, dice: Dice, screenBounds: BoundingBox) =
  //   if (gameTime.running.value < nextAsteroidSpawn || asteroids.length >= maxAsteroids)
  //     this
  //   else {
  //     val percentThroughLevel = verticalOffset / targetVerticalOffset
  //     val minAsteroidSpawnRate =
  //       initMinAsteroidSpawnRate - (
  //         (targetMinAsteroidSpawnRate - initMinAsteroidSpawnRate) * percentThroughLevel
  //       )
  //     val maxAsteroidSpawnRate =
  //       initMaxAsteroidSpawnRate - (
  //         (targetMaxAsteroidSpawnRate - initMaxAsteroidSpawnRate) * percentThroughLevel
  //       )

  //     this.copy(
  //       asteroids =
  //         Asteroid.initial
  //           .moveTo(dice.rollDouble * screenBounds.width - 16, screenBounds.y - 20)
  //           .withRotation(dice.rollDouble * 360)
  //           .withRotationSpeed(dice.rollDouble)
  //           :: asteroids,
  //       nextAsteroidSpawn = gameTime.running.value + (dice.rollDouble * (maxAsteroidSpawnRate - minAsteroidSpawnRate) + minAsteroidSpawnRate)
  //     )
  //   }

  def isInMoonBelt: Boolean =
    ship.coords.y > -course.height && ship.coords.y < -(course.height - Belt.Moon.height)

  def updateRunningGame(gameTime: GameTime /*, dice: Dice*/, shipControl: ShipControl) = {
    // val verticalSpeed = Math.max(initialSpeed, targetVerticalSpeed * (verticalOffset / targetVerticalOffset))
    // val verticalDelta = -(Math.max(-3, Math.min(-1, ship.force.y)) * verticalSpeed) * gameTime.delta.value
    val nextShip =
      ship
        .update(
          gameTime,
          asteroids.map(_.getBoundingBox),
          course.givePlatforms(screenBounds),
          shipControl,
          BoundingBox(
            screenBounds.x.toDouble,
            screenBounds.y.toDouble,
            screenBounds.width.toDouble,
            screenBounds.height.toDouble
          ),
          course.height,
          isInMoonBelt
        )

    Outcome(
      this
        .copy(
          ship = nextShip,
          asteroids = asteroids.map(_.update),
          // asteroids = asteroids
          //   .map(_.moveBy(0, verticalDelta))
          //   .filter(a =>
          //     a.coords.x > screenBounds.x - a.boundingBox.width
          //       && a.coords.x < screenBounds.x + screenBounds.width
          //       && a.coords.y < screenBounds.y + screenBounds.height
          //   ),
          // verticalOffset = Math.min(targetVerticalOffset, verticalOffset + verticalDelta),
          timeRemainingInSeconds = Seconds(Math.max(0, (timeRemainingInSeconds - gameTime.delta).toDouble)),
          camera =
            if (nextShip.health > 0)
              camera.update(nextShip.coords.toPoint, nextShip.angle, course.height, screenBounds.height)
            else
              camera
        )
      // .spawnAsteroid(gameTime, dice, screenBounds)
    )
  }

  def presentTime: String = {
    val intSeconds            = timeRemainingInSeconds.toInt
    val minutes               = intSeconds / 60
    val seconds               = intSeconds % 60
    val minutesToShow: String = if (minutes < 10) "0" + minutes.toString else minutes.toString
    val secondsToShow: String = if (seconds < 10) "0" + seconds.toString else seconds.toString

    minutesToShow + ":" + secondsToShow
  }

  def percentComplete: Double =
    Math.floor((100 * -(ship.coords.y / course.height.toDouble)) * 100) / 100

  def toggleDebug: Game =
    this.copy(debugMode = !debugMode)
}

object Game {
  val maxTimeLimit: Seconds       = Seconds(300) // 5 Minutes
  val targetVerticalSpeed: Double = 400
  val initialSpeed: Double        = 40

  def initial(dice: Dice, screenBounds: Rectangle): Game = {
    val course =
      Course(List(Belt.Backyard, Belt.Sky, Belt.EmptySpace, Belt.Asteroids(3), Belt.Asteroids(6), Belt.Asteroids(9), Belt.Asteroids(2), Belt.EmptySpace, Belt.Moon))

    def createAsteroids: List[Asteroid] = {
      val res = course.belts
        .foldLeft(BuildingAsteroids.empty) {
          case (acc, belt) =>
            belt match {
              case b @ Belt.Asteroids(_) =>
                acc.copy(
                  asteroids = acc.asteroids ++ b.getAsteroids(dice, screenBounds.width, acc.heightSoFar),
                  heightSoFar = acc.heightSoFar + b.height
                )

              case b =>
                acc.copy(heightSoFar = acc.heightSoFar + b.height)
            }
        }

      res.asteroids
    }
    // .foldLeft(List.empty[(Belt, Int)]) { case (l, b) =>
    //   l.headOption match {
    //     case Some((b1, height)) =>
    //       (b, height + b1.height) :: l

    //     case None =>
    //       List((b, 0))
    //   }
    // }
    // .map(b => (b._1.getObstacles(dice, screenBounds.width), b._2))
    // .flatMap(t =>
    //   t._1
    //     .map(o =>
    //       Asteroid.initial
    //         .moveTo(o.x, o.y - t._2)
    //         .withRotation(dice.rollDouble * 360)
    //         .withRotationSpeed(dice.rollDouble)
    //         .withType(
    //           dice.roll(4) match {
    //             case 1 => AsteroidType.Small
    //             case 2 => AsteroidType.Medium
    //             case 3 => AsteroidType.Big
    //             case _ => AsteroidType.ThatsNoMoon
    //           }
    //         )
    //     )
    // )

    Game(
      LevelType.Lander,
      GameState.GameRunning,
      Ship.initial(screenBounds),
      maxTimeLimit,
      createAsteroids,
      // initialSpeed,
      // targetVerticalSpeed,
      // 0,
      // 0,
      // Game.maxTimeLimit.toDouble * 2 * Game.initialSpeed,
      screenBounds,
      course,
      Camera.initial,
      debugMode = false
    )
  }

  final case class BuildingAsteroids(asteroids: List[Asteroid], heightSoFar: Int)
  object BuildingAsteroids {
    def empty: BuildingAsteroids =
      BuildingAsteroids(Nil, 0)
  }
}

sealed trait GameState
object GameState {
  case object GameWin     extends GameState
  case object GameLoss    extends GameState
  case object GameRunning extends GameState
  case object GamePaused  extends GameState
}
