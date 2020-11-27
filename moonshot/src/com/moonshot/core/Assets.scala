package com.moonshot.core

import indigo._

object Assets {

  val spriteSheetName =
    AssetName("rocket")

  def loadingAssets(assetPath: String): Set[AssetType] =
    Font.assets(assetPath)

  def dynamicAssets(assetPath: String): Set[AssetType] =
    Placeholder.assets(assetPath) ++
      Rocket.assets(assetPath)

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

    def assets(assetPath: String): Set[AssetType] =
      Set(
        AssetType.Image(AssetName("squares"), AssetPath(assetPath + "assets/squares.png"))
      )

  }

  object Rocket {

    def assets(assetPath: String): Set[AssetType] =
      Set(
        AssetType.Image(spriteSheetName, AssetPath(assetPath + "assets/rocket.png"))
      )

    val thrustAnimationKey: AnimationKey =
      AnimationKey("thrust")

    val thrustAnimation: Animation =
      Animation(
        thrustAnimationKey,
        Material.Textured(spriteSheetName),
        Frame(Rectangle(32, 64, 32, 32), Millis(100)),
        Frame(Rectangle(32, 96, 32, 32), Millis(100)),
        Frame(Rectangle(32, 128, 32, 32), Millis(100))
      )

  }

  object Font {
    val smallFontName: AssetName = AssetName("smallFontName")
    val fontKey: FontKey         = FontKey("boxy font")

    def assets(assetPath: String): Set[AssetType] =
      Set(AssetType.Image(smallFontName, AssetPath(assetPath + "assets/boxy_font_small.png")))

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
        .addChar(FontChar("%", 47, 0, 14, 12))
  }
}

object Prefabs {

  val rocket: Graphic =
    Graphic(Rectangle(0, 0, 64, 64), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(32, 0, 64, 64)
      .withRef(32, 32)

  val fumes: NonEmptyList[SceneGraphNode] =
    NonEmptyList
      .fromList[SceneGraphNode](
        (0 to 5).toList.map { i =>
          Graphic(Rectangle(0, 0, 32, 32), 10, Material.Textured(Assets.spriteSheetName))
            .withCrop(0, 32 * i, 32, 32)
            .withRef(16, 16)
        }
      )
      .getOrElse(throw new Exception("Couldn't create clouds"))

  val moon: Graphic =
    Graphic(Rectangle(0, 0, 192, 192), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(320, 0, 192, 192)
      .withRef(96, 96)

  val asteroid1: Graphic =
    Graphic(Rectangle(0, 0, 17, 17), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(161, 1, 17, 17)
      .withRef(8, 8)

  val asteroid2: Graphic =
    Graphic(Rectangle(0, 0, 29, 30), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(162, 33, 29, 30)
      .withRef(15, 15)

  val asteroid3: Graphic =
    Graphic(Rectangle(0, 0, 61, 61), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(194, 2, 61, 61)
      .withRef(30, 30)

  val asteroid4: Graphic =
    Graphic(Rectangle(0, 0, 125, 126), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(162, 65, 125, 126)
      .withRef(63, 63)

  val asteroids: NonEmptyList[Graphic] =
    NonEmptyList(asteroid1, asteroid2, asteroid3, asteroid4)

  val cloud1: Graphic =
    Graphic(Rectangle(0, 0, 64, 32), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(0, 224, 64, 32)
      .withRef(32, 16)

  val cloud2: Graphic =
    Graphic(Rectangle(0, 0, 128, 32), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(0, 265, 128, 32)
      .withRef(64, 16)

  val cloud3: Graphic =
    Graphic(Rectangle(0, 0, 160, 64), 1, Material.Textured(Assets.spriteSheetName))
      .withCrop(0, 288, 160, 64)
      .withRef(80, 32)

  val clouds: NonEmptyList[Graphic] =
    NonEmptyList(cloud1, cloud2, cloud3)

  val swatches: List[Graphic] =
    (0 to 11).toList.map { i =>
      Graphic(Rectangle(0, 0, 32, 32), 100, Material.Textured(Assets.spriteSheetName))
        .withCrop(576, 32 * i, 32, 32)
    }

  val swatch1: Graphic  = swatches(10)
  val swatch2: Graphic  = swatches(9)
  val swatch3: Graphic  = swatches(8)
  val swatch4: Graphic  = swatches(7)
  val swatch5: Graphic  = swatches(6)
  val swatch6: Graphic  = swatches(5)
  val swatch7: Graphic  = swatches(4)
  val swatch8: Graphic  = swatches(3)
  val swatch9: Graphic  = swatches(2)
  val swatch10: Graphic = swatches(1)
  val swatch11: Graphic = swatches(0)
}
