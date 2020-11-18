package com.moonshot.core

import indigo._

object Assets {

  val assets: Set[AssetType] =
    Placeholder.assets ++
      Rocket.assets

  object Placeholder {
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

  object Rocket {

    val name =
      AssetName("rocket")

    val assets: Set[AssetType] =
      Set(
        AssetType.Image(name, AssetPath("assets/rocket.png"))
      )

    val rocket: Graphic =
      Graphic(Rectangle(0, 0, 64, 64), 1, Material.Textured(name))
        .withCrop(32, 0, 64, 64)
        .withRef(32, 32)

    val clouds: NonEmptyList[SceneGraphNode] =
      NonEmptyList
        .fromList[SceneGraphNode](
          (0 to 5).toList.map { i =>
            Graphic(Rectangle(0, 0, 32, 32), 10, Material.Textured(name))
              .withCrop(0, 32 * i, 32, 32)
              .withRef(16, 16)
          }
        )
        .getOrElse(throw new Exception("Couldn't create clouds"))

  }

}
