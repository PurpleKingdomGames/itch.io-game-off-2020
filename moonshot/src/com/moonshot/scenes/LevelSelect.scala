package com.moonshot.scenes

import indigo._
import indigo.scenes._
import com.moonshot.core.StartUpData
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.model.LevelType
import com.moonshot.viewmodel.ViewInfo
import com.moonshot.model.LevelType.Lander
import com.moonshot.model.LevelType.Slalom
import com.moonshot.model.LevelType.Gauntlet
// import indigo.shared.events.ToggleFullScreen

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
    // case KeyboardEvent.KeyUp(Key.KEY_F) =>
    //   Outcome(model)
    //     .addGlobalEvents(ToggleFullScreen)

    case KeyboardEvent.KeyUp(Key.UP_ARROW) =>
      Outcome(model.up)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.down)

    case KeyboardEvent.KeyUp(Key.ENTER) =>
      model match {
        case Lander =>
          Outcome(model)
            .addGlobalEvents(SceneEvent.JumpTo(Level.name))

        case Slalom =>
          Outcome(model)
            .addGlobalEvents(SceneEvent.JumpTo(Level.name))

        case Gauntlet =>
          Outcome(model)
      }

    case _ =>
      Outcome(model)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: LevelType, viewModel: ViewInfo): GlobalEvent => Outcome[ViewInfo] =
    ViewInfo.fullScreenToggleProcessing(viewModel).orElse {
      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: LevelType, viewModel: ViewInfo): SceneUpdateFragment = {
    val middle = viewModel.giveScreenBounds.center
    SceneUpdateFragment(
      Text("Moonshot", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -90)
        .withTint(0, 0, 125)
        .alignCenter,
      Text("Left/Right arrows rotate", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -60)
        .alignCenter,
      Text("Up Arrow for Thrust", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -30)
        .alignCenter,
      Text("Avoid Asteroids and land on the moon!", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, 0)
        .alignCenter,
      Text("Hit enter to begin", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, 60)
        .alignCenter
    )
      .withAudio(
        SceneAudio(
          SceneAudioSource(
            BindingKey(Assets.Sounds.engineLoop.value),
            PlaybackPattern.SingleTrackLoop(
              Track(Assets.Sounds.menuLoop, Volume(0.75))
            )
          )
        )
      )
      .withMagnification(viewModel.magnification)
  }
}
