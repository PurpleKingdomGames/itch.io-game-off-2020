package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.Assets
import com.moonshot.core.StartUpData
import com.moonshot.model.Fumes
import com.moonshot.viewmodel.ViewModel
import com.moonshot.model.{Ship, ShipControl}
import com.moonshot.viewmodel.ViewInfo
import indigo.shared.datatypes.TextAlignment
import com.moonshot.model.GameState

object Level extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = ViewModel

  def name: SceneName =
    SceneName("level")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set(Fumes.subSystem)

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] = {
    case ResetLevel =>
      Outcome(Game.initial(context.startUpData.screenBounds))

    case e =>
      model.update(context.gameTime, context.dice, context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle), context.startUpData.screenBounds)(e)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    ViewInfo.fullScreenToggleViewModel(viewModel).orElse {

      case FrameTick =>
        if (context.running - viewModel.level.fumesLastSpawn > Seconds(0.025)) {
          val fumeEvents =
            if (context.inputState.keyboard.keysAreDown(Key.UP_ARROW))
              List(
                Fumes.spawn(
                  model.ship.toScreenSpace,
                  Seconds(context.dice.rollDouble * 0.5 + 0.5),
                  Radians(model.ship.angle.value + ((context.dice.rollDouble * 0.2) - 0.1))
                )
              )
            else Nil

          val sceneEvents =
            if (model.ship.lastDeath != Seconds.zero && ((context.running - model.ship.lastDeath).value * 0.5) >= 1)
              List(SceneEvent.JumpTo(Customisation.name), StartCustomisationAt(context.running))
            else
              Nil

          Outcome(
            viewModel.copy(
              level = viewModel.level.copy(
                fumesLastSpawn = context.running
              )
            )
          )
            .addGlobalEvents(fumeEvents)
            .addGlobalEvents(sceneEvents)

        } else Outcome(viewModel)

      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val running              = context.gameTime.running
    val textWaitTime: Double = 3

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
        viewModel.viewInfo.gameViewport.horizontalMiddle / 2,
        viewModel.viewInfo.gameViewport.verticalMiddle / 2,
        0,
        Assets.Font.fontKey
      )
        .withAlignment(TextAlignment.Center)
        .withAlpha(
          if (viewModel.level.firstLoad == Seconds.zero)
            0
          else if ((running.value - viewModel.level.firstLoad.value) < textWaitTime)
            1
          else
            Math.max(0, 1 - ((running.value - viewModel.level.firstLoad.value - textWaitTime) * 0.3))
        )

    val asteroidGraphic = Assets.Placeholder.blueBox

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
          Text(model.presentTime, context.startUpData.screenBounds.toRectangle.right - 10, 10, 0, Assets.Font.fontKey).alignRight,
          openingText,
          Text(
            "Paused",
            viewModel.viewInfo.gameViewport.horizontalMiddle / 2,
            viewModel.viewInfo.gameViewport.verticalMiddle / 2,
            0,
            Assets.Font.fontKey
          )
            .withAlignment(TextAlignment.Center)
            .withAlpha(
              if (model.gameState == GameState.GamePaused)
                1
              else
                0
            )
        )
      )
      .withGameColorOverlay(
        if (model.ship.lastDeath == Seconds.zero)
          RGBA.Zero
        else
          RGBA.Black.withAmount(Math.min(1, (running - model.ship.lastDeath).value * 0.5))
      )
      .withMagnification(viewModel.viewInfo.magnification)
  }

}

final case object ResetLevel extends GlobalEvent
