package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.model.{Model, Game}
import com.moonshot.core.StartUpData
import com.moonshot.model.Fumes
import com.moonshot.viewmodel.ViewModel
import com.moonshot.model.{Ship, ShipControl}
import com.moonshot.viewmodel.ViewInfo

object Level extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = ViewModel

  def name: SceneName =
    SceneName("level")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  def eventFilters: EventFilters =
    EventFilters.Default

  def subSystems: Set[SubSystem] =
    Set(Fumes.subSystem)

  def goToCustomisation(ship: Ship, runningTime: Seconds): Boolean =
    ship.lastDeath != Seconds.zero && ((runningTime - ship.lastDeath).value * 0.5) >= 1

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] = {
    case ResetLevel =>
      Outcome(Game.initial(context.startUpData.screenBounds))

    case FrameTick if goToCustomisation(model.ship, context.running) =>
      Outcome(
        model,
        List(SceneEvent.JumpTo(Customisation.name), StartCustomisationAt(context.running))
      )

    case e =>
      model.update(context.gameTime /*, context.dice*/, context.inputState.mapInputs(Ship.inputMappings, ShipControl.Idle), context.startUpData.screenBounds)(e)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    ViewInfo.fullScreenToggleViewModel(viewModel).orElse {
      case FrameTick if context.running - viewModel.level.fumesLastSpawn > Seconds(0.025) =>
        val fumeEvents =
          if (context.inputState.keyboard.keysAreDown(Key.UP_ARROW))
            List(
              Fumes.spawn(
                model.ship.toScreenSpace,
                Seconds(context.dice.rollDouble * 0.5 + 0.5),
                Radians(model.ship.angle.value + ((context.dice.rollDouble * 0.2) - 0.1))
              )
            )
          else Nil

        Outcome(
          viewModel.copy(
            level = viewModel.level.copy(
              fumesLastSpawn = context.running
            )
          ),
          fumeEvents
        )

      case _ =>
        Outcome(viewModel)
    }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: ViewModel): SceneUpdateFragment =
    LevelView.present(context, model, viewModel)

}

final case object ResetLevel extends GlobalEvent
