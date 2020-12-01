package com.moonshot.scenes

import indigo._
import indigo.scenes._
import com.moonshot.core.StartUpData
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.viewmodel.ViewInfo

object Title extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Unit
  type SceneViewModel = ViewInfo

  def name: SceneName =
    SceneName("title")

  def modelLens: Lens[Model, Unit] =
    Lens(_ => (), (m, _) => m)

  def viewModelLens: Lens[ViewModel, ViewInfo] =
    ViewInfo.lens

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], model: Unit): GlobalEvent => Outcome[Unit] = {
    case KeyboardEvent.KeyUp(Key.ENTER) =>
      Outcome(model)
        .addGlobalEvents(SceneEvent.JumpTo(Level.name))

    case _ =>
      Outcome(model)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: Unit, viewModel: ViewInfo): GlobalEvent => Outcome[ViewInfo] =
    ViewInfo.fullScreenToggleProcessing(viewModel).orElse {
      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Unit, viewModel: ViewInfo): SceneUpdateFragment = {
    val middle = viewModel.giveScreenBounds.center

    SceneUpdateFragment(
      Assets.Backgrounds.retryBg,
      Assets.Backgrounds.retryGrass,
      Text("Moonshot", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -80)
        .withTint(0, 0, 125)
        .alignCenter,
      Text("Left/Right arrows rotate", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -50)
        .alignCenter,
      Text("Up Arrow for Thrust", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, -20)
        .alignCenter,
      Text("Avoid Asteroids and land on the moon!", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, 0)
        .alignCenter
    ).addGameLayerNodes(
      Signal
        .Pulse(Millis(400).toSeconds)
        .map { p =>
          if (p)
            List(
              Text("Hit enter to begin", 0, 0, 0, Assets.Font.fontKey)
                .moveTo(middle)
                .moveBy(0, 90)
                .alignCenter
            )
          else Nil
        }
        .at(context.running)
    ).withAudio(
      SceneAudio(
        SceneAudioSource(
          BindingKey(Assets.Sounds.engineLoop.value),
          PlaybackPattern.SingleTrackLoop(
            Track(Assets.Sounds.menuLoop, Volume(0.75))
          )
        )
      )
    ).withMagnification(viewModel.magnification)
  }
}
