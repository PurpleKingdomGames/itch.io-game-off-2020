package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.Assets
import com.moonshot.core.StartUpData

object Level extends Scene[StartUpData, Model, Unit] {

  type SceneModel     = Game
  type SceneViewModel = Unit

  def name: SceneName =
    SceneName("level")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[Unit, Unit] =
    Lens.keepOriginal

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] =
    e => model.update(context.gameTime, context.dice)(e)

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[StartUpData], model: Game, viewModel: Unit): SceneUpdateFragment = {
    val shipGraphic = Assets
      .redBox
      .moveTo(model.ship.toScreenSpace)

    val asteroidGraphic = Assets
      .blueBox

    SceneUpdateFragment(shipGraphic)
      .addGameLayerNodes(
        model.asteroids.map(a => asteroidGraphic
          .moveTo(a.coords.x.toInt, a.coords.y.toInt)
        )
      )
  }

}
