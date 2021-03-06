package com.moonshot

import indigo._
import indigo.scenes._

import com.moonshot.model.Model
import com.moonshot.core.StartUpData
import com.moonshot.core.Assets

import scala.scalajs.js.annotation.JSExportTopLevel
import com.moonshot.scenes.Level
import com.moonshot.scenes.Title
import com.moonshot.scenes.TryAgain
import com.moonshot.scenes.loading.Loading
import com.moonshot.viewmodel.ViewModel
import com.moonshot.scenes.Splash
import com.moonshot.viewmodel.ViewInfo

@JSExportTopLevel("IndigoGame")
object Moonshot extends IndigoGame[BootData, StartUpData, Model, ViewModel] {

  val eventFilters: EventFilters = EventFilters.Default

  def boot(flags: Map[String, String]): BootResult[BootData] = {
    val assetPath: String =
      flags.getOrElse("assetPath", "")

    val gameViewport =
      (flags.get("width"), flags.get("height")) match {
        case (Some(w), Some(h)) =>
          GameViewport(w.toInt, h.toInt)

        case _ =>
          GameViewport.at720p
      }

    val magnification: Int =
      ViewInfo.pickMagnification(gameViewport)

    val targetFPS = 60

    BootResult(
      GameConfig.default
        .withViewport(gameViewport)
        .withClearColor(ClearColor.fromHexString("0F033A"))
        .withMagnification(magnification)
        .withFrameRate(targetFPS),
      BootData(assetPath, gameViewport.giveDimensions(magnification), magnification, gameViewport)
    )
      .withAssets(Assets.loadingAssets(assetPath))
      .withFonts(Assets.Font.fontInfo)
      .withAnimations(Assets.Rocket.thrustAnimation)
  }

  def scenes(bootData: BootData): NonEmptyList[Scene[StartUpData, Model, ViewModel]] =
    NonEmptyList(
      Loading(bootData.assetPath, bootData.viewport),
      Splash,
      Title,
      Level,
      TryAgain
    )

  def initialScene(bootData: BootData): Option[SceneName] =
    None

  def setup(
      bootData: BootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Startup[StartUpData] =
    Startup.Success(
      StartUpData(
        bootData.startingMagnification,
        bootData.gameViewport,
        dice
      )
    )

  def initialModel(startupData: StartUpData): Model =
    Model.initial(startupData.dice, startupData.initialScreenBounds)

  def initialViewModel(startupData: StartUpData, model: Model): ViewModel =
    ViewModel.initial(startupData.magnification, startupData.gameViewport)

}

final case class BootData(assetPath: String, viewport: Rectangle, startingMagnification: Int, gameViewport: GameViewport)
