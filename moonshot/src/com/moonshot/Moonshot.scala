package com.moonshot

import indigo._

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Moonshot extends IndigoDemo[Unit, StartUpData, Game, Unit] {

  val magnification              = 1
  val eventFilters: EventFilters = EventFilters.Default

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assetName = AssetName("squares")
  val mainGraphic =
    Graphic(Rectangle(0, 0, 192, 32), 1, Material.Textured(assetName))

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(AssetName("squares"), AssetPath("assets/squares.png"))
    )

  val fonts: Set[FontInfo] =
    Set()

  def boot(flags: Map[String, String]): BootResult[Unit] =
    BootResult
      .noData(
        GameConfig.default
          .withViewport(550, 400)
          .withClearColor(ClearColor.fromRGB(1, 1, 1))
          .withMagnification(magnification)
      )
      .withAssets(assets)
      .withFonts(fonts)

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Startup[StartUpData] =
    Startup.Success(StartUpData())

  def initialModel(startupData: StartUpData): Game =
    Game.initial()

  def initialViewModel(startupData: StartUpData, model: Game): Unit =
    ()

  def updateModel(
      context: FrameContext[StartUpData],
      model: Game
  ): GlobalEvent => Outcome[Game] = {
    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[StartUpData],
      model: Game,
      viewModel: Unit
  ): SceneUpdateFragment =
    SceneUpdateFragment.empty

  def updateViewModel(
      context: FrameContext[StartUpData],
      model: Game,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

}

final case class StartUpData()