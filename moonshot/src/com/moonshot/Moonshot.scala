package com.moonshot

import indigo._
import indigo.scenes._

import com.moonshot.model.Model
import com.moonshot.core.StartUpData
import com.moonshot.core.Assets

import scala.scalajs.js.annotation.JSExportTopLevel
import com.moonshot.scenes.Level
import com.moonshot.viewmodel.ViewModel
import indigoextras.geometry.BoundingBox

@JSExportTopLevel("IndigoGame")
object Moonshot extends IndigoGame[BootData, StartUpData, Model, ViewModel] {

  val eventFilters: EventFilters = EventFilters.Default

  def boot(flags: Map[String, String]): BootResult[BootData] = {
    val gameViewport =
      (flags.get("width"), flags.get("height")) match {
        case (Some(w), Some(h)) =>
          GameViewport(w.toInt, h.toInt)

        case _ =>
          GameViewport.at720p
      }

    val magnification: Int =
      if (gameViewport == GameViewport.at1080p) 3
      else if (gameViewport == GameViewport.at720p) 2
      else 1

    BootResult(
      GameConfig.default
        .withViewport(gameViewport)
        .withClearColor(ClearColor.fromHexString("000D93"))
        .withMagnification(magnification)
        .withFrameRate(60),
      BootData(gameViewport.giveDimensions(magnification))
    )
      .withAssets(Assets.assets)
  }

  def scenes(bootData: BootData): NonEmptyList[Scene[StartUpData, Model, ViewModel]] =
    NonEmptyList(Level)

  def initialScene(bootData: BootData): Option[SceneName] =
    None

  def setup(
      bootData: BootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Startup[StartUpData] =
    Startup.Success(
      StartUpData(
        BoundingBox(
          bootData.viewport.x.toDouble,
          bootData.viewport.y.toDouble,
          bootData.viewport.width.toDouble,
          bootData.viewport.height.toDouble
        )
      )
    )

  def initialModel(startupData: StartUpData): Model =
    Model.initial(startupData.screenBounds)

  def initialViewModel(startupData: StartUpData, model: Model): ViewModel =
    ViewModel.initial

}

final case class BootData(viewport: Rectangle)
