package com.moonshot.scenes

import indigo._
import indigo.scenes._

import com.moonshot.core.Assets
import com.moonshot.model.{Model, Game}
import com.moonshot.core.StartUpData
import com.moonshot.viewmodel.ViewModel
import com.moonshot.viewmodel.ViewInfo
import com.moonshot.viewmodel.CustomisationViewModel
import com.moonshot.core.Prefabs
import com.moonshot.model.Fumes
import indigoextras.subsystems.AutomataEvent

object Customisation extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = Game
  type SceneViewModel = WorkingViewModel

  def name: SceneName =
    SceneName("customisation")

  def modelLens: Lens[Model, Game] =
    Lens(_.game, (m, g) => m.copy(game = g))

  def viewModelLens: Lens[ViewModel, WorkingViewModel] =
    WorkingViewModel.lens

  def eventFilters: EventFilters =
    EventFilters.Default
      .withViewModelFilter {
        case e: StartCustomisationAt =>
          Some(e)

        case e: KeyboardEvent.KeyUp =>
          Some(e)

        case e: ViewEvent =>
          Some(e)

        case FrameTick =>
          Some(FrameTick)

        case _ =>
          None
      }

  def subSystems: Set[SubSystem] =
    Set(Fumes.subSystem(150))

  def updateModel(context: FrameContext[StartUpData], model: Game): GlobalEvent => Outcome[Game] = {
    case _ =>
      Outcome(model)
  }

  def updateViewModel(context: FrameContext[StartUpData], model: Game, viewModel: WorkingViewModel): GlobalEvent => Outcome[WorkingViewModel] =
    e => {
      val default: PartialFunction[GlobalEvent, Outcome[ViewInfo]] =
        _ => Outcome(viewModel.viewInfo)

      val updatedViewInfo: Outcome[ViewInfo] =
        ViewInfo.fullScreenToggleProcessing(viewModel.viewInfo).orElse(default)(e)

      e match {
        case StartCustomisationAt(time) =>
          updatedViewInfo.map { newViewInfo =>
            viewModel
              .updateStartTime(time)
              .withViewInfo(newViewInfo)
          }

        case KeyboardEvent.KeyUp(Key.ENTER) if context.running - viewModel.customisation.screenEnteredAt > Seconds(2) =>
          Outcome(viewModel)
            .addGlobalEvents(
              SceneEvent.JumpTo(Level.name),
              ResetLevel,
              AutomataEvent.KillAll(Fumes.poolKey)
            )

        case _ =>
          updatedViewInfo.map(viewModel.withViewInfo(_))
      }
    }

  def present(context: FrameContext[StartUpData], model: Game, viewModel: WorkingViewModel): SceneUpdateFragment = {
    val middle =
      viewModel.viewInfo.giveScreenBounds.center

    val timeSinceEntered =
      context.running - viewModel.customisation.screenEnteredAt

    val fadeInDuration      = Seconds(1)
    val shipFallDuration    = Seconds(0.75)
    val startTextFlashAfter = fadeInDuration + shipFallDuration + Seconds(0.5)

    val shipGraphic =
      AnimationSignals
        .crashShip(middle, shipFallDuration)
        .map { pt =>
          Prefabs.rocket
            .rotate(Radians(3.6))
            .moveTo(pt)
        }
        .at(timeSinceEntered - fadeInDuration)

    val tryAgain =
      Text("Oops! Hit enter to try again!", 0, 0, 0, Assets.Font.fontKey)
        .moveTo(middle + Point(0, -40))
        .alignCenter
        .withAlpha(AnimationSignals.textFlash(startTextFlashAfter).at(timeSinceEntered))

    SceneUpdateFragment(
      Assets.Backgrounds.retryBg,
      shipGraphic,
      Assets.Backgrounds.retryGrass
    )
      .addUiLayerNodes(tryAgain)
      .withGameColorOverlay(AnimationSignals.fadeIn(fadeInDuration).at(timeSinceEntered))
      .withMagnification(viewModel.viewInfo.magnification)
      .addGlobalEvents(
        Fumes.spawn(
          shipGraphic.position,
          Seconds(1.0),
          Radians(Radians.TAUby2.value + ((context.dice.rollDouble * 0.5) - 0.25))
        )
      )
  }

}

final case class StartCustomisationAt(time: Seconds) extends GlobalEvent
final case class WorkingViewModel(customisation: CustomisationViewModel, viewInfo: ViewInfo) {
  def updateStartTime(time: Seconds): WorkingViewModel =
    this.copy(
      customisation = customisation.copy(screenEnteredAt = time)
    )

  def withViewInfo(newViewInfo: ViewInfo): WorkingViewModel =
    this.copy(viewInfo = newViewInfo)
}

object WorkingViewModel {

  val lens: Lens[ViewModel, WorkingViewModel] =
    Lens(
      vm => WorkingViewModel(vm.customisation, vm.viewInfo),
      (vm, wvm) => vm.copy(customisation = wvm.customisation, viewInfo = wvm.viewInfo)
    )

}
