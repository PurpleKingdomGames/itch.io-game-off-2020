package com.moonshot.scenes

import indigo._
import indigo.scenes._
import com.moonshot.core.StartUpData
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.Assets
import indigo.shared.events.EnterFullScreen
import indigo.shared.events.ExitFullScreen
import com.moonshot.Moonshot
import indigo.shared.events.FullScreenEntered
import indigo.shared.events.FullScreenExited

object FullScreen extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = WindowMode
  type SceneViewModel = ViewModel

  def name: SceneName =
    SceneName("fullscreen toggle")

  def modelLens: Lens[Model, WindowMode] =
    Lens(_.windowMode, (m, sm) => m.copy(windowMode = sm))

  def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], model: WindowMode): GlobalEvent => Outcome[WindowMode] = {
    case KeyboardEvent.KeyUp(Key.UP_ARROW) | KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      val next = model.toggle

      Outcome(next)
        .addGlobalEvents(if (next.isFullscreen) EnterFullScreen else ExitFullScreen)

    case KeyboardEvent.KeyUp(Key.ENTER) =>
      Outcome(model)
        .addGlobalEvents(SceneEvent.Next)

    case _ =>
      Outcome(model)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: WindowMode, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] = {
    case ViewportResize(gameViewport) =>
      Outcome(
        viewModel.copy(
          magnification = Moonshot.pickMagnification(gameViewport),
          gameViewport = gameViewport
        )
      )

    case FullScreenEntered =>
      Outcome(
        viewModel.copy(
          magnification = Moonshot.pickMagnification(viewModel.gameViewport)
        )
      )

    case FullScreenExited =>
      Outcome(
        viewModel.copy(
          magnification = Moonshot.pickMagnification(viewModel.gameViewport)
        )
      )

    case _ =>
      Outcome(viewModel)
  }

  def present(context: FrameContext[StartUpData], model: WindowMode, viewModel: ViewModel): SceneUpdateFragment =
    SceneUpdateFragment(
      Text(model.renderWindowed, 10, 10, 0, Assets.Font.fontKey),
      Text(model.renderFullscreen, 10, 30, 0, Assets.Font.fontKey),
      Text("Hit enter to continue", 10, 50, 0, Assets.Font.fontKey)
    ).withMagnification(viewModel.magnification)

}

sealed trait WindowMode {
  def isFullscreen: Boolean

  def renderWindowed: String
  def renderFullscreen: String

  def toggle: WindowMode =
    this match {
      case WindowMode.FullScreenMode => WindowMode.WindowedMode
      case WindowMode.WindowedMode   => WindowMode.FullScreenMode
    }
}
object WindowMode {
  case object WindowedMode extends WindowMode {
    def isFullscreen: Boolean    = false
    val renderWindowed: String   = "[x] Windowed"
    val renderFullscreen: String = "[_] Fullscreen"
  }
  case object FullScreenMode extends WindowMode {
    def isFullscreen: Boolean    = true
    val renderWindowed: String   = "[_] Windowed"
    val renderFullscreen: String = "[x] Fullscreen"
  }
}
