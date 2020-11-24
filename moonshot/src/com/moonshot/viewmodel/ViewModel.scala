package com.moonshot.viewmodel

import indigo.shared.time.Seconds
import indigo.shared.config.GameViewport
import indigo.shared.events.ViewportResize
import indigo.shared.Outcome
import indigo.shared.events.GlobalEvent
import indigo.scenes.Lens

final case class ViewModel(level: LevelViewModel, viewInfo: ViewInfo, customisation: CustomisationViewModel)
object ViewModel {

  def initial(magnification: Int, gameViewport: GameViewport): ViewModel =
    ViewModel(
      LevelViewModel.initial,
      ViewInfo.initial(magnification, gameViewport),
      CustomisationViewModel(Seconds.zero)
    )

}

final case class LevelViewModel(firstLoad: Seconds, fumesLastSpawn: Seconds)
object LevelViewModel {
  val initial: LevelViewModel =
    LevelViewModel(Seconds.zero, Seconds.zero)
}

final case class ViewInfo(magnification: Int, gameViewport: GameViewport)
object ViewInfo {
  def initial(magnification: Int, gameViewport: GameViewport): ViewInfo =
    ViewInfo(magnification, gameViewport)

  def lens: Lens[ViewModel, ViewInfo] =
    Lens(
      _.viewInfo,
      (vm, vi) => vm.copy(viewInfo = vi)
    )

  def pickMagnification(gameViewport: GameViewport): Int =
    gameViewport.height match {
      case 1440 => 3
      case 1080 => 3
      case 1024 => 3
      case 900  => 3
      case 864  => 3
      case 800  => 3
      case 768  => 2
      case 720  => 2
      case _    => 1
    }

  def fullScreenToggleViewModel(viewModel: ViewModel): PartialFunction[GlobalEvent, Outcome[ViewModel]] = {
    case e @ (ViewportResize(_)) =>
      fullScreenToggleProcessing(viewModel.viewInfo)(e).map(vi => viewModel.copy(viewInfo = vi))
  }

  def fullScreenToggleProcessing(viewInfo: ViewInfo): PartialFunction[GlobalEvent, Outcome[ViewInfo]] = {
    case ViewportResize(gameViewport) =>
      Outcome(
        viewInfo.copy(
          magnification = pickMagnification(gameViewport),
          gameViewport = gameViewport
        )
      )
  }
}

final case class CustomisationViewModel(screenEnteredAt: Seconds)
