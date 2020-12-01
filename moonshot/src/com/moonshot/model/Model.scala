package com.moonshot.model

import com.moonshot.scenes.loading.LoadingModel
import indigo.shared.datatypes.Rectangle
import indigo.shared.dice.Dice
import indigo.shared.time.Seconds

final case class Model(
    loadingScene: LoadingModel,
    timeOnSplashScreen: Seconds,
    game: Game
)

object Model {

  def initial(dice: Dice, screenBounds: Rectangle): Model =
    Model(
      LoadingModel.initial,
      Seconds.zero,
      Game.initial(dice, screenBounds)
    )

}
