package com.moonshot.scenes

import indigo._
import indigo.scenes._
import com.moonshot.core.StartUpData
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import com.moonshot.viewmodel.ViewInfo

object Splash extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Seconds
  type SceneViewModel = ViewInfo

  def name: SceneName =
    SceneName("splash")

  def modelLens: Lens[Model, Seconds] =
    Lens(_.timeOnSplashScreen, (m, t) => m.copy(timeOnSplashScreen = t))

  def viewModelLens: Lens[ViewModel, ViewInfo] =
    ViewInfo.lens

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], timeOnScreen: Seconds): GlobalEvent => Outcome[Seconds] = {
    case FrameTick if timeOnScreen >= Seconds(3) =>
      Outcome(timeOnScreen)
        .addGlobalEvents(SceneEvent.JumpTo(Title.name))

    case FrameTick =>
      Outcome(timeOnScreen + context.delta)

    case _ =>
      Outcome(timeOnScreen)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: Seconds, viewModel: ViewInfo): GlobalEvent => Outcome[ViewInfo] =
    ViewInfo.fullScreenToggleProcessing(viewModel).orElse {
      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Seconds, viewModel: ViewInfo): SceneUpdateFragment = {
    val middle = viewModel.giveScreenBounds.center

    SceneUpdateFragment(
      Assets.indigoLogo
        .moveTo(middle)
        .moveBy(0, -20),
      Text("Made with Indigo", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle)
        .moveBy(0, 45)
        .alignCenter
    )
      .withMagnification(viewModel.magnification)
  }
}
