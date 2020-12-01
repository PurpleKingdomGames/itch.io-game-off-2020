package com.moonshot.scenes.loading

import indigo._
import indigo.scenes._
import indigoextras.subsystems._

import indigo.scenes.SceneEvent
import com.moonshot.model.Model
import com.moonshot.viewmodel.ViewModel
import com.moonshot.core.StartUpData
import com.moonshot.core.Assets
import com.moonshot.scenes.Splash

final case class Loading(assetPath: String, screenDimensions: Rectangle) extends Scene[StartUpData, Model, ViewModel] {

  type SceneModel     = LoadingState
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("loading")

  val modelLens: Lens[Model, LoadingState] =
    Lens(
      m => m.loadingScene.loadingState,
      (m, sm) => m.copy(loadingScene = LoadingModel(sm))
    )

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens(_ => (), (vm, _) => vm)

  val eventFilters: EventFilters =
    EventFilters.Default

  val subSystems: Set[SubSystem] =
    Set(AssetBundleLoader)

  def updateModel(
      context: FrameContext[StartUpData],
      loadingState: LoadingState
  ): GlobalEvent => Outcome[LoadingState] = {
    case FrameTick =>
      loadingState match {
        case LoadingState.NotStarted =>
          Outcome(LoadingState.InProgress(0))
            .addGlobalEvents(
              AssetBundleLoaderEvent.Load(BindingKey("Loading"), Assets.dynamicAssets(assetPath))
            )

        case _ =>
          Outcome(loadingState)
      }

    case AssetBundleLoaderEvent.LoadProgress(_, percent, _, _) =>
      Outcome(LoadingState.InProgress(percent))

    case AssetBundleLoaderEvent.Success(_) =>
      Outcome(LoadingState.Complete)
        .addGlobalEvents(SceneEvent.JumpTo(Splash.name))

    case AssetBundleLoaderEvent.Failure(_, _) =>
      Outcome(LoadingState.Error)

    case _ =>
      Outcome(loadingState)
  }

  def updateViewModel(
      context: FrameContext[StartUpData],
      loadingState: LoadingState,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[StartUpData], loadingState: LoadingState, viewModel: Unit): SceneUpdateFragment =
    LoadingView.draw(
      screenDimensions,
      loadingState
    )

}
