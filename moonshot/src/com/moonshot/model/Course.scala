package com.moonshot.model

import indigo.shared.dice.Dice
import indigo.shared.datatypes.Vector2
import indigo.shared.datatypes.Point

final case class Course(belts: List[Belt]) {
  def length      = belts.length
  val height: Int = belts.map(_.height).sum
}

sealed trait Belt {
  val height: Int = Belt.standardHeight
  def getObstacles(dice: Dice, width: Int): List[Point]
}
object Belt {
  val standardHeight: Int = 500

  case object Backyard extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Point] = Nil
  }
  case object Moon extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Point] = Nil
  }

  case object Sky extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Point] =
      Belt
        .getObstacles(dice, width, 10, 80, 32)
        .filter(o => o.y <= standardHeight)
  }
  case object EmptySpace extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Point] = Nil
  }

  case object Asertoids extends Belt {
    def getObstacles(dice: Dice, width: Int): List[Point] =
      Belt
        .getObstacles(dice, width, 40, 64, 32)
        .filter(o => o.y <= standardHeight)
  }

  private def getObstacles(dice: Dice, width: Int, numObstacles: Int, spaceBetweenX: Double, spaceBetweenY: Double) =
    filterCollisions(
      spaceBetweenX,
      spaceBetweenY,
      buildObstacleRows(
        dice,
        numObstacles,
        spaceBetweenX,
        spaceBetweenY,
        List
          .range(0, (width.doubleValue / spaceBetweenX).toInt)
          .map(i => new Vector2((dice.rollDouble * i * spaceBetweenX) + (i * spaceBetweenX * 0.75), 0)),
        Nil
      ),
      Nil
    ).map(o => o.toPoint)

  private def buildObstacleRows(dice: Dice, numObstacles: Int, spaceBetweenX: Double, spaceBetweenY: Double, lastRow: List[Vector2], currentObstacles: List[Vector2]): List[Vector2] = {
    val currentRow =
      lastRow.map { o =>
        val startPoint = new Vector2(o.x, o.y + spaceBetweenY * 2)
        val a          = dice.rollDouble * 2 * Math.PI
        val r          = spaceBetweenX * Math.sqrt(dice.rollDouble)

        val x = r * Math.cos(a)
        val y = r * Math.sin(a)

        new Vector2(startPoint.x + x, startPoint.y + y)
      }

    val newObstacles = currentRow ++ currentObstacles
    if (newObstacles.length < numObstacles)
      buildObstacleRows(dice, numObstacles, spaceBetweenX, spaceBetweenY, currentRow, newObstacles)
    else
      newObstacles.take(numObstacles)
  }

  private def filterCollisions(spaceBetweenX: Double, spaceBetweenY: Double, obstacles: List[Vector2], checkedObstacles: List[Vector2]): List[Vector2] =
    obstacles.headOption match {
      case Some(o) =>
        filterCollisions(
          spaceBetweenX,
          spaceBetweenY,
          obstacles
            .filter(o2 =>
              Math.max(o2.x, o.x) - Math.min(o2.x, o.x) >= spaceBetweenX &&
                Math.max(o2.y, o.y) - Math.min(o2.y, o.y) >= spaceBetweenY
            ),
          o :: checkedObstacles
        )
      case None => obstacles
    }
}
