package com.moonshot.viewmodel

import indigo.shared.time.Seconds
import indigo.shared.config.GameViewport

final case class ViewModel(level: LevelViewModel, magnification: Int, gameViewport: GameViewport)
object ViewModel {

  def initial(magnification: Int, gameViewport: GameViewport): ViewModel =
    ViewModel(LevelViewModel(Seconds.zero), magnification, gameViewport)

}

final case class LevelViewModel(fumesLastSpawn: Seconds)
