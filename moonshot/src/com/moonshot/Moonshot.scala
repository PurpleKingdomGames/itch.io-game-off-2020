package com.moonshot

import indigo._
import indigo.scenes._

import com.moonshot.model.Model
import com.moonshot.core.StartUpData
import com.moonshot.core.Assets

import scala.scalajs.js.annotation.JSExportTopLevel
import com.moonshot.scenes.Level

@JSExportTopLevel("IndigoGame")
object Moonshot extends IndigoGame[Unit, StartUpData, Model, Unit] {

  val eventFilters: EventFilters = EventFilters.Default

  def boot(flags: Map[String, String]): BootResult[Unit] =
    BootResult
      .noData(
        GameConfig.default
          .withViewport(550, 400)
          .withClearColor(ClearColor.fromRGB(1, 1, 1))
          .withMagnification(1)
      )
      .withAssets(Assets.assets)

  def scenes(bootData: Unit): NonEmptyList[Scene[StartUpData, Model, Unit]] =
    NonEmptyList(Level)

  def initialScene(bootData: Unit): Option[SceneName] =
    None // or Some(Level.name)

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Startup[StartUpData] =
    Startup.Success(StartUpData())

  def initialModel(startupData: StartUpData): Model =
    Model.initial

  def initialViewModel(startupData: StartUpData, model: Model): Unit =
    ()

}
