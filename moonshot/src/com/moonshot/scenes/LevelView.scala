package com.moonshot.scenes

import indigo._
import com.moonshot.core.StartUpData
import com.moonshot.model.Game
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.model.Ship
import com.moonshot.model.GameState
import com.moonshot.model.Camera
import com.moonshot.viewmodel.ViewInfo
import com.moonshot.model.Course
import com.moonshot.core.Prefabs
import com.moonshot.model.AsteroidType.Small
import com.moonshot.model.AsteroidType.Medium
import com.moonshot.model.AsteroidType.Big
import com.moonshot.model.AsteroidType.ThatsNoMoon
import com.moonshot.model.ShipControl
import com.moonshot.model.ShipControl.Thrust
import com.moonshot.model.ShipControl.ThrustLeft
import com.moonshot.model.ShipControl.ThrustRight

object LevelView {

  def pointToScreenSpace(camera: Camera, viewInfo: ViewInfo): Point => Point =
    _ + camera.inverse + Point(0, (viewInfo.giveScreenBounds.height / 2).toInt)

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val boundPadding                  = 128
    val toScreenSpace: Point => Point = pointToScreenSpace(model.camera, viewModel.viewInfo)
    val running                       = context.gameTime.running
    val renderBounds                  = new Rectangle(new Point(-boundPadding / 2, -boundPadding / 2), model.screenBounds.size + boundPadding)

    val shipGraphic = {
      val s = Prefabs.rocket
        .moveTo(toScreenSpace(model.ship.toScreenSpace))
        .rotate(model.ship.angle)

      if (running - model.ship.lastImpact < Ship.invulnerableFor)
        Signal
          .Pulse(Millis(100).toSeconds)
          .map { p =>
            if (p) s.withOverlay(Overlay.Color(RGBA.White))
            else s
          }
          .at(running)
      else s
    }

    val combinedShip =
      if (context.inputState.keyboard.keysAreDown(Key.UP_ARROW))
        List(
          Prefabs.thrust
            .moveTo(toScreenSpace(model.ship.toScreenSpace))
            .rotate(model.ship.angle),
          shipGraphic
        )
      else
        List(shipGraphic)

    val parallaxY: Int =
      -(1080 - 360 - ((1080 - 360) * (model.percentComplete / 100))).toInt

    val bg =
      List(
        Assets.Backgrounds.starsBg,
        Assets.Backgrounds.parallaxStars.moveTo(0, parallaxY)
      )

    drawCourse(model.course, model.screenBounds, toScreenSpace, model.debugMode) |+|
      SceneUpdateFragment(bg ++ combinedShip)
        .addGameLayerNodes(
          model.asteroids
            .filter(a => renderBounds.isPointWithin(toScreenSpace(a.coords.toPoint)))
            .map { a =>
              val graphic = (a._type match {
                case Small =>
                  Prefabs.asteroid1
                case Medium =>
                  Prefabs.asteroid2
                case Big =>
                  Prefabs.asteroid3
                case ThatsNoMoon =>
                  Prefabs.asteroid4
              })
                .moveTo(toScreenSpace(a.coords.toPoint))
                .rotateBy(a.rotation)

              (a.coords.x + a.coords.y).toInt % 3 match {
                case 0 => graphic.withOverlay(Overlay.Color(RGBA.Black.withAmount(0.5)))
                case 1 =>
                  graphic.withOverlay(Overlay.Color(RGBA.White.withAmount(0.5)))
                case _ => graphic
              }
            }
        )
        .addUiLayerNodes(drawUI(model /*, viewModel*/, viewModel.viewInfo.giveScreenBounds, running))
        .withGameColorOverlay(
          if (model.ship.lastDeath == Seconds.zero)
            RGBA.Zero
          else
            RGBA.Black.withAmount(Math.min(1, (running - model.ship.lastDeath).value * 0.5))
        )
        .withMagnification(viewModel.viewInfo.magnification)
        .withAudio(addAudio(context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle)))
  }

  def drawCourse(course: Course, screenSize: Rectangle, toScreenSpace: Point => Point, debugMode: Boolean): SceneUpdateFragment =
    SceneUpdateFragment.empty
      .addGameLayerNodes {
        course.belts
          .foldLeft((List.empty[SceneGraphNode], 0)) {
            case ((nodes, verticalOffset), belt) =>
              (nodes ++ belt.background(screenSize, verticalOffset, toScreenSpace), verticalOffset - belt.height)
          }
          ._1
      }
      .addUiLayerNodes( // Debug only - remove later
        if (debugMode)
          course.belts
            .foldLeft((List.empty[SceneGraphNode], 0)) {
              case ((nodes, verticalOffset), belt) =>
                val box = Assets.Placeholder.redBox

                val redPosition: Point   = Point(0, -belt.height + verticalOffset)
                val greenPosition: Point = Point(0, verticalOffset - box.lazyBounds.height)

                IndigoLogger.debugOnce(redPosition.toString() + " : " + greenPosition.toString())

                val res =
                  List(
                    box
                      .moveTo(toScreenSpace(redPosition)),
                    box
                      .moveTo(toScreenSpace(greenPosition))
                      .withOverlay(Overlay.Color(RGBA.Green))
                  )

                (nodes ++ res, verticalOffset - belt.height)
            }
            ._1
        else Nil
      )

  def drawUI(model: Game, screenSize: Rectangle, running: Seconds): List[SceneGraphNode] = {
    val middle = screenSize.center

    val endText =
      model.gameState match {
        case GameState.GameWin =>
          List(
            Text("You won!", 0, 0, 0, Assets.Font.fontKey)
              .moveTo(middle)
              .alignCenter,
            Text("Hit enter to try again!", 0, 0, 0, Assets.Font.fontKey)
              .moveTo(middle)
              .moveBy(0, 32)
              .alignCenter
              .withAlpha(AnimationSignals.textFlash(Seconds(1)).at(running))
          )
        case GameState.GameLoss =>
          List(
            Text("Hit enter to try again!", 0, 0, 0, Assets.Font.fontKey)
              .moveTo(middle)
              .moveBy(0, 32)
              .alignCenter
              .withAlpha(AnimationSignals.textFlash(Seconds(1)).at(running))
          )
        case _ =>
          Nil
      }

    val debug =
      if (model.debugMode)
        List(
          Text(
            model.percentComplete.toString + " / 100",
            screenSize.right - 10,
            30,
            0,
            Assets.Font.fontKey
          ).alignRight,
          Text(screenSize.width.toString() + " x " + screenSize.height.toString, screenSize.right - 10, 50, 0, Assets.Font.fontKey).alignRight,
          Text(model.ship.coords.toPoint.toString(), screenSize.right - 10, 70, 0, Assets.Font.fontKey).alignRight,
          Text(model.ship.angle.value.toString().take(4) + " yaw", screenSize.right - 10, 90, 0, Assets.Font.fontKey).alignRight,
          Text("In moon belt: " + model.isInMoonBelt.toString(), screenSize.right - 10, 110, 0, Assets.Font.fontKey).alignRight
        )
      else Nil

    List(
      Text("Health: " + model.ship.health.toString(), 10, 10, 0, Assets.Font.fontKey),
      Text("Distance to Moon\n" + model.distanceToMoon.toInt.toString + " km", screenSize.right - 10, 10, 0, Assets.Font.fontKey).alignRight,
      Text("Paused", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .alignCenter
        .withAlpha(if (model.gameState == GameState.GamePaused) 1 else 0)
    ) ++ endText ++ debug
  }

  def addAudio(shipControl: ShipControl): SceneAudio = {
    val thrustSound =
      SceneAudioSource(
        BindingKey(Assets.Sounds.engineLoop.value),
        PlaybackPattern.SingleTrackLoop(
          Track(Assets.Sounds.engineLoop, Volume(0.25))
        )
      )

    val bgMusic =
      SceneAudioSource(
        BindingKey(Assets.Sounds.mainLoop.value),
        PlaybackPattern.SingleTrackLoop(
          Track(Assets.Sounds.mainLoop, Volume(0.75))
        )
      )

    shipControl match {
      case Thrust =>
        SceneAudio(thrustSound, bgMusic)

      case ThrustLeft =>
        SceneAudio(thrustSound, bgMusic)

      case ThrustRight =>
        SceneAudio(thrustSound, bgMusic)

      case _ =>
        SceneAudio(SceneAudioSource.None, bgMusic)
    }
  }
}
