package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.core.Assets
import com.moonshot.model.{Model, Game}
import com.moonshot.core.StartUpData
import com.moonshot.viewmodel.ViewModel
import com.moonshot.model.{Ship, ShipControl}
import com.moonshot.viewmodel.ViewInfo
import com.moonshot.model.GameState

object Customisation extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = ViewModel

  def name: SceneName =
    SceneName("customisation")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set.empty

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] =
    e => {
      val screenBounds = context.startUpData.screenBounds;
      val biggerBounds =
        screenBounds
          .copy(
            position = screenBounds.position.withY(-64),
            size = screenBounds.size.withY(screenBounds.height + 64)
          )

      if (model.gameState != GameState.ShipCustomisation)
        Outcome(
          model
            .withState(GameState.ShipCustomisation)
            .copy(
              ship = Ship
                .initial(biggerBounds)
                .copy(coords = new Vector2((biggerBounds.width * 0.5) - 32, -64))
            )
        )
      else
        model
          .update(context.gameTime, context.dice, context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle), biggerBounds)(e)
    }
  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    ViewInfo.fullScreenToggleViewModel(viewModel).orElse {
      case FrameTick =>
        if (viewModel.level.firstLoad == Seconds.zero)
          Outcome(viewModel.copy(level = viewModel.level.copy(firstLoad = context.running)))
        else
          Outcome(viewModel)

      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment = {
    val shipGraphic = Assets.Rocket.rocket
      .moveTo(model.ship.toScreenSpace)

    SceneUpdateFragment(shipGraphic)
  }

}
