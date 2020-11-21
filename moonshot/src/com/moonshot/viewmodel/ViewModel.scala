package com.moonshot.viewmodel

import indigo.shared.time.Seconds
import indigo.shared.config.GameViewport

final case class ViewModel(fumesLastSpawn: Seconds, magnification: Int, gameViewport: GameViewport)
object ViewModel {

  def initial(magnification: Int, gameViewport: GameViewport): ViewModel =
    ViewModel(Seconds.zero, magnification, gameViewport)

}
