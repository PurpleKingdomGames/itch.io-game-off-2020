package com.moonshot.model

import indigoextras.geometry.BoundingBox
import com.moonshot.scenes.loading.LoadingModel
import com.moonshot.scenes.WindowMode

final case class Model(
    loadingScene: LoadingModel,
    windowMode: WindowMode,
    game: Game
)

object Model {

  def initial(screenBounds: BoundingBox): Model =
    Model(
      LoadingModel.initial,
      WindowMode.WindowedMode,
      Game.initial(screenBounds)
    )

}
