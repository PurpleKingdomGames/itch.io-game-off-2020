package com.moonshot.core

import indigo._

object Assets {

  val assetName =
    AssetName("squares")

  val mainGraphic =
    Graphic(Rectangle(0, 0, 192, 32), 1, Material.Textured(assetName))

  val redBox =
    Graphic(Rectangle(0, 0, 192, 32), 1, Material.Textured(assetName))
      .withCrop(Rectangle(32, 0, 32, 32))

  val blueBox =
    Graphic(Rectangle(0, 0, 192, 32), 1, Material.Textured(assetName))
      .withCrop(Rectangle(128, 0, 32, 32))

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(AssetName("squares"), AssetPath("assets/squares.png"))
    )

}
