package com.moonshot.model

import indigo.shared.dice.Dice
import indigo.shared.datatypes.Vector2
import indigoextras.geometry.LineSegment
import indigoextras.geometry.Vertex

final case class Course(belts: List[Belt]) {
  def length      = belts.length
  val height: Int = belts.map(_.height).sum

  def givePlatforms: List[LineSegment] =
    belts.zipWithIndex.flatMap {
      case (b, i) =>
        b.getPlatforms.map { ls =>
          val moveBy = -(b.height * i).toDouble

          LineSegment(
            ls.start.translate(Vertex(0, moveBy)),
            ls.end.translate(Vertex(0, moveBy))
          )
        }
    }
}

sealed trait Belt {
  val height: Int = Belt.standardHeight
  def getObstacles(dice: Dice, width: Int): List[Vector2]
  def getPlatforms: List[LineSegment]
}
object Belt {
  val standardHeight: Int = 2000

  case object Backyard extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Vector2] = Nil

    def getPlatforms: List[LineSegment] =
      List(
        LineSegment(Vertex(-100, 1), Vertex(500, 1))
      )
  }

  case object Moon extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Vector2] = Nil
    def getPlatforms: List[LineSegment]                     = Nil
  }

  case object Sky extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 80, 32)
        .filter(o => o.y <= standardHeight)

    def getPlatforms: List[LineSegment] =
      List(
        LineSegment(Vertex(0, 0), Vertex(100, 0))
      )
  }

  case object EmptySpace extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 64, 32)
        .filter(o => o.y <= standardHeight)

    def getPlatforms: List[LineSegment] = Nil
  }

  case object Asertoids extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Vector2] =
      Belt
        .getObstacles(dice, width, height, 64, 32)
        .filter(o => o.y <= standardHeight)

    def getPlatforms: List[LineSegment] = Nil
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
          .map(i => new Vector2((dice.rollDouble * i * spaceBetweenX) + (i * spaceBetweenX * 0.75), 0)),
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
    if (newObstacles.map(_.y).min > -standardHeight)
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
