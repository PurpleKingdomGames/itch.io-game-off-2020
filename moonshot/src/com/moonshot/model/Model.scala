package com.moonshot.model

final case class Model(game: Game)

object Model {

  val initial: Model =
    Model(Game.initial)

}
