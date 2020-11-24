package com.moonshot.model

import com.moonshot.scenes.loading.LoadingModel
import com.moonshot.scenes.WindowMode
import indigo.shared.datatypes.Rectangle

final case class Model(
    loadingScene: LoadingModel,
    windowMode: WindowMode,
    game: Game
)

object Model {

  def initial(screenBounds: Rectangle): Model =
    Model(
      LoadingModel.initial,
      WindowMode.WindowedMode,
      Game.initial(screenBounds)
    )

}
