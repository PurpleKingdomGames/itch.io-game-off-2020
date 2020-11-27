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

object LevelView {

  def pointToScreenSpace(camera: Camera, viewInfo: ViewInfo): Point => Point =
    _ + camera.inverse + Point(0, (viewInfo.giveScreenBounds.height / 2).toInt)

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val toScreenSpace: Point => Point = pointToScreenSpace(model.camera, viewModel.viewInfo)
    val running                       = context.gameTime.running
    val renderBounds                  = new Rectangle(new Point(-16, -16), model.screenBounds.size + 32)

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

    val asteroidGraphic = Assets.Placeholder.blueBox

    SceneUpdateFragment(combinedShip)
      .addGameLayerNodes(drawCourse(model.course, model.screenBounds, toScreenSpace))
      .addGameLayerNodes(
        model.asteroids
          .filter(a => renderBounds.isPointWithin(toScreenSpace(a.coords.toPoint)))
          .map(a =>
            asteroidGraphic
              .moveTo(toScreenSpace(a.coords.toPoint))
          )
      )
      .addUiLayerNodes(drawUI(model, viewModel, viewModel.viewInfo.giveScreenBounds, running))
      .withGameColorOverlay(
        if (model.ship.lastDeath == Seconds.zero)
          RGBA.Zero
        else
          RGBA.Black.withAmount(Math.min(1, (running - model.ship.lastDeath).value * 0.5))
      )
      .withMagnification(viewModel.viewInfo.magnification)
  }

  def drawCourse(course: Course, screenSize: Rectangle, toScreenSpace: Point => Point): List[SceneGraphNode] =
    course.belts.zipWithIndex.flatMap {
      case (belt, index) =>
        belt.background(screenSize, -((belt.height * index).toInt), toScreenSpace)
    }

  def drawUI(model: Game, viewModel: ViewModel, screenSize: Rectangle, running: Seconds): List[SceneGraphNode] = {
    val middle               = screenSize.center
    val textWaitTime: Double = 3

    val openingText =
      Text(
        "5 minutes to Dinner!",
        middle.x / 2,
        middle.y / 2,
        0,
        Assets.Font.fontKey
      ).alignCenter
        .withAlpha(
          if (viewModel.level.firstLoad == Seconds.zero)
            0
          else if ((running.value - viewModel.level.firstLoad.value) < textWaitTime)
            1
          else
            Math.max(0, 1 - ((running.value - viewModel.level.firstLoad.value - textWaitTime) * 0.3))
        )

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
            Text("Dinner time! You lost!", 0, 0, 0, Assets.Font.fontKey)
              .moveTo(middle)
              .alignCenter,
            Text("Hit enter to try again!", 0, 0, 0, Assets.Font.fontKey)
              .moveTo(middle)
              .moveBy(0, 32)
              .alignCenter
              .withAlpha(AnimationSignals.textFlash(Seconds(1)).at(running))
          )
        case _ =>
          Nil
      }

    List(
      Text("Health: " + model.ship.health.toString(), 10, 10, 0, Assets.Font.fontKey),
      Text(model.presentTime, screenSize.right - 10, 10, 0, Assets.Font.fontKey).alignRight,
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
      openingText,
      Text("Paused", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .alignCenter
        .withAlpha(if (model.gameState == GameState.GamePaused) 1 else 0)
    ) ++ endText
  }

}
