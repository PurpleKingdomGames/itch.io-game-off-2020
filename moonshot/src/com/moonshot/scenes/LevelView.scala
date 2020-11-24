package com.moonshot.scenes

import indigo._
import com.moonshot.core.StartUpData
import com.moonshot.model.Game
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.model.Ship
import com.moonshot.model.GameState

object LevelView {

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val running              = context.gameTime.running
    val textWaitTime: Double = 3

    val screenSize: Rectangle =
      viewModel.viewInfo.giveScreenBounds

    val middle =
      screenSize.center

    val shipGraphic = {
      val s = Assets.Rocket.rocket
        .moveTo(model.ship.toScreenSpace)
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

    val asteroidGraphic = Assets.Placeholder.blueBox

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
              .withAlpha(AnimationSignals.textFlash(Seconds(1)).at(context.running))
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
              .withAlpha(AnimationSignals.textFlash(Seconds(1)).at(context.running))
          )
        case _ =>
          Nil
      }

    SceneUpdateFragment(shipGraphic)
      .addGameLayerNodes(
        model.asteroids.map(a =>
          asteroidGraphic
            .moveTo(a.coords.x.toInt, a.coords.y.toInt)
        )
      )
      .addUiLayerNodes(
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
          openingText,
          Text("Paused", 0, 0, 0, Assets.Font.fontKey)
            .moveTo(middle)
            .alignCenter
            .withAlpha(if (model.gameState == GameState.GamePaused) 1 else 0)
        )
      )
      .addUiLayerNodes(endText)
      .withGameColorOverlay(
        if (model.ship.lastDeath == Seconds.zero)
          RGBA.Zero
        else
          RGBA.Black.withAmount(Math.min(1, (running - model.ship.lastDeath).value * 0.5))
      )
      .withMagnification(viewModel.viewInfo.magnification)
  }

}
