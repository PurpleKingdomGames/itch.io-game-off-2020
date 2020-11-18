package com.moonshot.viewmodel

import indigo.shared.time.Seconds

final case class ViewModel(level: LevelViewModel)
object ViewModel {

  val initial: ViewModel =
    ViewModel(LevelViewModel(Seconds.zero))

}

final case class LevelViewModel(fumesLastSpawn: Seconds)
