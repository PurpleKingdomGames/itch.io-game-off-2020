package com.moonshot.viewmodel

import indigo.shared.time.Seconds
import indigo.shared.config.GameViewport
import indigo.shared.events.ViewportResize
import indigo.shared.Outcome
import indigo.shared.events.GlobalEvent
import indigo.shared.events.FullScreenExited
import indigo.shared.events.FullScreenEntered
import indigo.scenes.Lens

final case class ViewModel(level: LevelViewModel, viewInfo: ViewInfo)
object ViewModel {

  def initial(magnification: Int, gameViewport: GameViewport): ViewModel =
    ViewModel(
      LevelViewModel.initial,
      ViewInfo.initial(magnification, gameViewport)
    )

}

final case class LevelViewModel(fumesLastSpawn: Seconds)
object LevelViewModel {
  val initial: LevelViewModel =
    LevelViewModel(Seconds.zero)
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
    if (gameViewport.height >= GameViewport.at1080p.height) 3
    else if (gameViewport.height >= GameViewport.at720p.height) 2
    else 1

  def fullScreenToggleViewModel(viewModel: ViewModel): PartialFunction[GlobalEvent, Outcome[ViewModel]] = {
    case e @ (FullScreenEntered | FullScreenExited | ViewportResize(_)) =>
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

    case FullScreenEntered =>
      Outcome(
        viewInfo.copy(
          magnification = pickMagnification(viewInfo.gameViewport)
        )
      )

    case FullScreenExited =>
      Outcome(
        viewInfo.copy(
          magnification = pickMagnification(viewInfo.gameViewport)
        )
      )
  }
}
