package com.moonshot.model

import indigoextras.geometry.LineSegment
import indigoextras.geometry.Vertex
import com.moonshot.core.Prefabs
import indigo._
import com.moonshot.core.Assets

final case class Course(belts: List[Belt]) {
  def length      = belts.length
  val height: Int = belts.map(_.height).sum

  def givePlatforms(screenSize: Rectangle): List[LineSegment] =
    belts.zipWithIndex.flatMap {
      case (b, i) =>
        b.getPlatforms(screenSize).map { ls =>
          val moveBy = -(b.height * i).toDouble

          LineSegment(
            ls.start.translate(Vertex(0, moveBy)),
            ls.end.translate(Vertex(0, moveBy))
          )
        }
    }
}

sealed trait Belt {
  val height: Int

  def getObstacles(dice: Dice, width: Int): List[Vector2]
  def getPlatforms(screenSize: Rectangle): List[LineSegment]

  def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode]
}
object Belt {

  case object Backyard extends Belt {
    val height: Int = 500

    def getObstacles(dice: Dice, width: Int): List[Vector2] = Nil

    def getPlatforms(screenSize: Rectangle): List[LineSegment] =
      List(
        LineSegment(Vertex(-100, 1), Vertex(500, 1))
      )

    def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode] = {

      val ground =
        Assets.Placeholder.redBox
          .moveTo(toScreenSpace(Point(0, 0)))
          .scaleBy(640 / 32, 1)
          .withOverlay(Overlay.Color(RGBA(0.9, 0.9, 0.9, 1.0)))

      val bgColour =
        Prefabs.swatch1
          .moveTo(toScreenSpace(Point(0, -height + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, height.toDouble / 32.0)

      List(
        bgColour,
        ground
      )
    }
  }

  case object Moon extends Belt {
    val height: Int = 500

    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Nil

    def getPlatforms(screenSize: Rectangle): List[LineSegment] =
      List(
        LineSegment(
          Vertex(
            x = screenSize.width.toDouble / 2 - 64,
            y = -(height.toDouble / 2)
          ),
          Vertex(
            x = screenSize.width.toDouble / 2 + 64,
            y = -(height.toDouble / 2)
          )
        )
      )

    def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode] =
      List(
        Prefabs.moon
          .moveTo(toScreenSpace(Point(screenSize.width / 2, -(height / 2) + verticalOffset)))
      )
  }

  case object Sky extends Belt {
    val height: Int = 500

    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 80, 32)
        .filter(o => o.y <= height)

    def getPlatforms(screenSize: Rectangle): List[LineSegment] =
      Nil

    def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode] =
      List(
        Prefabs.swatch6
          .moveTo(toScreenSpace(Point(0, (-32 * 6) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1),
        Prefabs.swatch5
          .moveTo(toScreenSpace(Point(0, (-32 * 5) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1),
        Prefabs.swatch4
          .moveTo(toScreenSpace(Point(0, (-32 * 4) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1),
        Prefabs.swatch3
          .moveTo(toScreenSpace(Point(0, (-32 * 3) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1),
        Prefabs.swatch2
          .moveTo(toScreenSpace(Point(0, (-32 * 2) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1),
        Prefabs.swatch1
          .moveTo(toScreenSpace(Point(0, (-32 * 1) + verticalOffset)))
          .scaleBy(screenSize.width.toDouble / 32.0, 1)
      )
  }

  case object EmptySpace extends Belt {
    val height: Int = 500

    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 64, 32)
        .filter(o => o.y <= height)

    def getPlatforms(screenSize: Rectangle): List[LineSegment] =
      Nil

    def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode] =
      Nil
  }

  case object Asertoids extends Belt {
    val height: Int = 1000

    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 64, 32)
        .filter(o => o.y <= height)

    def getPlatforms(screenSize: Rectangle): List[LineSegment] =
      Nil

    def background(screenSize: Rectangle, verticalOffset: Int, toScreenSpace: Point => Point): List[SceneGraphNode] =
      Nil
  }

  private def getObstacles(dice: Dice, width: Int, height: Int, spaceBetweenX: Double, spaceBetweenY: Double) =
    filterCollisions(
      spaceBetweenX,
      spaceBetweenY,
      buildObstacleRows(
        dice,
        height,
        spaceBetweenX,
        spaceBetweenY,
        List
          .range(0, (width.doubleValue / spaceBetweenX).toInt)
          .map { i =>
            val min = i * spaceBetweenX * 0.9
            val max = i * spaceBetweenX
            new Vector2((dice.rollDouble * (max - min)) + min, 0)
          },
        Nil
      ),
      Nil
    )

  private def buildObstacleRows(dice: Dice, height: Int, spaceBetweenX: Double, spaceBetweenY: Double, lastRow: List[Vector2], currentObstacles: List[Vector2]): List[Vector2] = {
    val currentRow =
      lastRow.map { o =>
        val startPoint = new Vector2(o.x, o.y - spaceBetweenY * 2)
        val a          = dice.rollDouble * 2 * Math.PI
        val r          = spaceBetweenX * Math.sqrt(dice.rollDouble)

        val x = r * Math.cos(a)
        val y = r * Math.sin(a)

        new Vector2(startPoint.x + x, startPoint.y - y)
      }

    val newObstacles = currentRow ++ currentObstacles
    if (newObstacles.map(_.y).min > -height)
      buildObstacleRows(dice, height, spaceBetweenX, spaceBetweenY, currentRow, newObstacles)
    else
      newObstacles.filter(o => o.y > -height)
  }

  private def filterCollisions(spaceBetweenX: Double, spaceBetweenY: Double, obstacles: List[Vector2], checkedObstacles: List[Vector2]): List[Vector2] =
    obstacles.headOption match {
      case Some(o) =>
        filterCollisions(
          spaceBetweenX,
          spaceBetweenY,
          obstacles
            .filter(o2 =>
              o2 != o &&
                Math.max(o2.x, o.x) - Math.min(o2.x, o.x) >= spaceBetweenX &&
                Math.max(o2.y, o.y) - Math.min(o2.y, o.y) >= spaceBetweenY
            ),
          o :: checkedObstacles
        )
      case None => checkedObstacles
    }
}
