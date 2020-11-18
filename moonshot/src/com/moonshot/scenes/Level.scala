package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.Assets
import com.moonshot.core.StartUpData
import com.moonshot.model.Fumes
import com.moonshot.viewmodel.ViewModel
import com.moonshot.viewmodel.LevelViewModel

object Level extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = LevelViewModel

  def name: SceneName =
    SceneName("level")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, LevelViewModel] =
    Lens(_.level, (vm, l) => vm.copy(level = l))

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set(Fumes.subSystem)

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] =
    e => model.update(context.gameTime, context.dice)(e)

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: LevelViewModel): GlobalEvent => Outcome[LevelViewModel] = {
    case FrameTick =>
      if (context.running - viewModel.fumesLastSpawn > Seconds(0.025)) {
        val fumeEvents =
          if (context.inputState.keyboard.keysAreDown(Key.UP_ARROW))
            List(
              Fumes.spawn(
                model.ship.toScreenSpace,
                Seconds(context.dice.rollDouble * 0.5 + 0.5),
                Radians(0.0 + ((context.dice.rollDouble * 0.2) - 0.1))
              )
            )
          else Nil

        Outcome(viewModel.copy(fumesLastSpawn = context.running))
          .addGlobalEvents(fumeEvents)

      } else Outcome(viewModel)

    case _ =>
      Outcome(viewModel)
  }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: LevelViewModel): SceneUpdateFragment = {
    val shipGraphic = Assets.Rocket.rocket
      .moveTo(model.ship.toScreenSpace)

    val asteroidGraphic = Assets.Placeholder.blueBox

    SceneUpdateFragment(shipGraphic)
      .addGameLayerNodes(
        model.asteroids.map(a =>
          asteroidGraphic
            .moveTo(a.coords.x.toInt, a.coords.y.toInt)
        )
      )
  }

}
