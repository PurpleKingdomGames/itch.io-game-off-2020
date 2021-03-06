package com.moonshot.model

import indigo._
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.shared.time.GameTime
import indigoextras.geometry.BoundingBox
import com.moonshot.viewmodel.ScreenBoundsUpdated
import com.moonshot.model.Course
import com.moonshot.model.Belt
import com.moonshot.model.Camera
import com.moonshot.core.Assets

final case class Game(
    gameState: GameState,
    ship: Ship,
    timeRemainingInSeconds: Seconds,
    asteroids: List[Asteroid],
    screenBounds: Rectangle,
    course: Course,
    camera: Camera,
    distanceToMoon: Double,
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

  def isInMoonBelt: Boolean =
    ship.coords.y > -course.height && ship.coords.y < -(course.height - Belt.Moon.height)

  def updateRunningGame(gameTime: GameTime /*, dice: Dice*/, shipControl: ShipControl) = {
    val prevHealth = ship.health
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

    val courseHeightMinusMoon = (course.height - (course.belts
      .filter {
        _ match {
          case Belt.Moon =>
            true
          case _ => false
        }
      }
      .map(_.height)
      .sum)).toDouble
    val percentComplete = -(ship.coords.y / courseHeightMinusMoon)

    val distanceToMoon =
      if (nextShip.health > 0)
        Math.max(0, (Game.distanceFromEarthtoMoon - (Game.distanceFromEarthtoMoon * percentComplete)))
      else
        this.distanceToMoon

    val outcome = Outcome(
      this
        .copy(
          ship = nextShip,
          asteroids = asteroids.map(_.update),
          timeRemainingInSeconds = Seconds(Math.max(0, (timeRemainingInSeconds - gameTime.delta).toDouble)),
          distanceToMoon = distanceToMoon,
          camera =
            if (nextShip.health > 0)
              camera.update(nextShip.coords.toPoint, nextShip.angle, course.height, screenBounds.height)
            else
              camera
        )
    )

    if (prevHealth > nextShip.health)
      outcome
        .addGlobalEvents(
          PlaySound(
            if (nextShip.health > 0)
              Assets.Sounds.asteroidHit
            else
              Assets.Sounds.zeroHealth,
            Volume(0.25)
          )
        )
    else
      outcome
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
  val distanceFromEarthtoMoon     = 384399.9
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

    Game(
      GameState.GameRunning,
      Ship.initial(screenBounds),
      maxTimeLimit,
      createAsteroids,
      screenBounds,
      course,
      Camera.initial,
      distanceFromEarthtoMoon,
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
