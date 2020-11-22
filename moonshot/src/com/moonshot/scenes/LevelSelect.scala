package com.moonshot.scenes

import indigo._
import indigo.scenes._
import com.moonshot.core.StartUpData
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.model.LevelType
import com.moonshot.viewmodel.ViewInfo

object LevelSelect extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = LevelType
  type SceneViewModel = ViewInfo

  def name: SceneName =
    SceneName("level select")

  def modelLens: Lens[Model, LevelType] =
    Lens(_.game.levelType, (m, sm) => m.copy(game = m.game.copy(levelType = sm)))

  def viewModelLens: Lens[ViewModel, ViewInfo] =
    ViewInfo.lens

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], model: LevelType): GlobalEvent => Outcome[LevelType] = {
    case KeyboardEvent.KeyUp(Key.UP_ARROW) =>
      Outcome(model.up)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.down)

    case KeyboardEvent.KeyUp(Key.ENTER) =>
      Outcome(model)
        .addGlobalEvents(SceneEvent.JumpTo(Level.name))

    case _ =>
      Outcome(model)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: LevelType, viewModel: ViewInfo): GlobalEvent => Outcome[ViewInfo] =
    ViewInfo.fullScreenToggleProcessing(viewModel).orElse {
      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: LevelType, viewModel: ViewInfo): SceneUpdateFragment =
    SceneUpdateFragment(
      Text("Choose a level type:", 10, 10, 0, Assets.Font.fontKey),
      Text(model.renderLander, 10, 30, 0, Assets.Font.fontKey),
      Text(model.renderSlalom, 10, 50, 0, Assets.Font.fontKey),
      Text(model.renderGauntlet, 10, 70, 0, Assets.Font.fontKey),
      Text("Hit enter to continue", 10, 90, 0, Assets.Font.fontKey)
    ).withMagnification(viewModel.magnification)

}
