package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.Assets
import com.moonshot.core.StartUpData
import com.moonshot.model.Fumes
import com.moonshot.viewmodel.ViewModel
import com.moonshot.model.{Ship, ShipControl}
import com.moonshot.Moonshot

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

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] =
    e => model.update(context.gameTime, context.dice, context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle), context.startUpData.screenBounds)(e)

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    Moonshot.fullScreenToggleProcessing(viewModel).orElse {
      case FrameTick =>
        if (context.running - viewModel.fumesLastSpawn > Seconds(0.025)) {
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

          Outcome(
            viewModel.copy(
              fumesLastSpawn = context.running
            )
          )
            .addGlobalEvents(fumeEvents)

        } else Outcome(viewModel)

      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val shipGraphic = {
      val s = Assets.Rocket.rocket
        .moveTo(model.ship.toScreenSpace)
        .rotate(model.ship.angle)

      val running = context.gameTime.running

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

    val asteroidGraphic = Assets.Placeholder.blueBox

    SceneUpdateFragment(shipGraphic)
      .addGameLayerNodes(
        model.asteroids.map(a =>
          asteroidGraphic
            .moveTo(a.coords.x.toInt, a.coords.y.toInt)
        )
      )
      .addGameLayerNodes(
        List(
          Text("Lives: " + model.ship.lives.toString(), 10, 10, 0, Assets.Font.fontKey),
          Text("Health: " + model.ship.health.toString(), 10, 30, 0, Assets.Font.fontKey),
          Text(model.presentTime, context.startUpData.screenBounds.toRectangle.right - 10, 10, 0, Assets.Font.fontKey).alignRight
        )
      )
      .withMagnification(viewModel.magnification)
  }

}
