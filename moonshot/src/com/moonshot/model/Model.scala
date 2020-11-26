package com.moonshot.model

import com.moonshot.scenes.loading.LoadingModel
import com.moonshot.scenes.WindowMode
import indigo.shared.datatypes.Rectangle
import indigo.shared.dice.Dice

final case class Model(
    loadingScene: LoadingModel,
    windowMode: WindowMode,
    game: Game
)

object Model {

  def initial(dice: Dice, screenBounds: Rectangle): Model =
    Model(
      LoadingModel.initial,
      WindowMode.WindowedMode,
      Game.initial(dice, screenBounds)
    )

}
