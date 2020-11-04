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
    e => model.update()(e)

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[StartUpData], model: Game, viewModel: Unit): SceneUpdateFragment = {
    val redBox =
      Signal
        .product(
          Signal.SmoothPulse.map(d => Vector2(1 + d, 1 + d)),
          Signal.CosWave.map(Radians.apply)
        )
        .map {
          case (scaleBy, rotateBy) =>
            Assets.redBox
              .moveTo(100, 100)
              .scaleBy(scaleBy)
              .rotate(rotateBy)
        }
        .at(context.gameTime.running)

    SceneUpdateFragment(Assets.mainGraphic, redBox)
  }

}
