package com.moonshot

import indigo._
import indigo.scenes._

import com.moonshot.model.Model
import com.moonshot.core.StartUpData
import com.moonshot.core.Assets

import scala.scalajs.js.annotation.JSExportTopLevel
import com.moonshot.scenes.Level
import com.moonshot.viewmodel.ViewModel

@JSExportTopLevel("IndigoGame")
object Moonshot extends IndigoGame[Unit, StartUpData, Model, ViewModel] {

  val eventFilters: EventFilters = EventFilters.Default

  def boot(flags: Map[String, String]): BootResult[Unit] =
    BootResult
      .noData(
        GameConfig.default
          .withViewport(350, 700)
          .withClearColor(ClearColor.fromHexString("000D93"))
          .withMagnification(1)
          .withFrameRate(60)
      )
      .withAssets(Assets.assets)

  def scenes(bootData: Unit): NonEmptyList[Scene[StartUpData, Model, ViewModel]] =
    NonEmptyList(Level)

  def initialScene(bootData: Unit): Option[SceneName] =
    None

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Startup[StartUpData] =
    Startup.Success(StartUpData())

  def initialModel(startupData: StartUpData): Model =
    Model.initial

  def initialViewModel(startupData: StartUpData, model: Model): ViewModel =
    ViewModel.initial

}
