package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.StartUpData
import com.moonshot.viewmodel.ViewModel
import com.moonshot.model.{Ship, ShipControl}

object Customisation extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = ViewModel

  def name: SceneName =
    SceneName("customisation")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] =
    e => model.update(context.gameTime, context.dice, context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle), context.startUpData.screenBounds)(e)

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] = {
    case _ =>
      Outcome(viewModel)
  }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment =
    SceneUpdateFragment.empty

}
