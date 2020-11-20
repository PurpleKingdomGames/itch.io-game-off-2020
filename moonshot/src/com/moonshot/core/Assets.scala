package com.moonshot.core

import indigo._

object Assets {

  val assets: Set[AssetType] =
    Placeholder.assets ++
      Rocket.assets ++
      Font.assets

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

  object Font {
    val smallFontName: AssetName = AssetName("smallFontName")
    val fontKey: FontKey         = FontKey("boxy font")
    val assets                   = Set(AssetType.Image(smallFontName, AssetPath("assets/boxy_font_small.png")))

    val fontInfo: FontInfo =
      FontInfo(fontKey, Material.Textured(smallFontName), 320, 230, FontChar("?", 47, 26, 11, 12))
        .addChar(FontChar("A", 2, 39, 10, 12))
        .addChar(FontChar("B", 14, 39, 9, 12))
        .addChar(FontChar("C", 25, 39, 10, 12))
        .addChar(FontChar("D", 37, 39, 9, 12))
        .addChar(FontChar("E", 49, 39, 9, 12))
        .addChar(FontChar("F", 60, 39, 9, 12))
        .addChar(FontChar("G", 72, 39, 9, 12))
        .addChar(FontChar("H", 83, 39, 9, 12))
        .addChar(FontChar("I", 95, 39, 5, 12))
        .addChar(FontChar("J", 102, 39, 9, 12))
        .addChar(FontChar("K", 113, 39, 10, 12))
        .addChar(FontChar("L", 125, 39, 9, 12))
        .addChar(FontChar("M", 136, 39, 13, 12))
        .addChar(FontChar("N", 2, 52, 11, 12))
        .addChar(FontChar("O", 15, 52, 10, 12))
        .addChar(FontChar("P", 27, 52, 9, 12))
        .addChar(FontChar("Q", 38, 52, 11, 12))
        .addChar(FontChar("R", 51, 52, 10, 12))
        .addChar(FontChar("S", 63, 52, 9, 12))
        .addChar(FontChar("T", 74, 52, 11, 12))
        .addChar(FontChar("U", 87, 52, 10, 12))
        .addChar(FontChar("V", 99, 52, 9, 12))
        .addChar(FontChar("W", 110, 52, 13, 12))
        .addChar(FontChar("X", 125, 52, 9, 12))
        .addChar(FontChar("Y", 136, 52, 11, 12))
        .addChar(FontChar("Z", 149, 52, 10, 12))
        .addChar(FontChar("0", 2, 13, 10, 12))
        .addChar(FontChar("1", 13, 13, 7, 12))
        .addChar(FontChar("2", 21, 13, 9, 12))
        .addChar(FontChar("3", 33, 13, 9, 12))
        .addChar(FontChar("4", 44, 13, 9, 12))
        .addChar(FontChar("5", 56, 13, 9, 12))
        .addChar(FontChar("6", 67, 13, 9, 12))
        .addChar(FontChar("7", 79, 13, 9, 12))
        .addChar(FontChar("8", 90, 13, 10, 12))
        .addChar(FontChar("9", 102, 13, 9, 12))
        .addChar(FontChar("?", 47, 26, 11, 12))
        .addChar(FontChar("!", 2, 0, 6, 12))
        .addChar(FontChar(".", 143, 0, 6, 12))
        .addChar(FontChar(",", 124, 0, 8, 12))
        .addChar(FontChar("-", 133, 0, 9, 12))
        .addChar(FontChar(" ", 112, 13, 12, 12))
        .addChar(FontChar("[", 2, 65, 7, 12))
        .addChar(FontChar("]", 21, 65, 7, 12))
        .addChar(FontChar("(", 84, 0, 7, 12))
        .addChar(FontChar(")", 93, 0, 7, 12))
        .addChar(FontChar("\\", 11, 65, 8, 12))
        .addChar(FontChar("/", 150, 0, 9, 12))
        .addChar(FontChar(":", 2, 26, 5, 12))
        .addChar(FontChar("_", 42, 65, 9, 12))
  }
}
