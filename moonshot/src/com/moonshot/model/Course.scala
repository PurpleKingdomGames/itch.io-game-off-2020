package com.moonshot.model

final case class Course(belts: List[Belt]) {
  def length      = belts.length
  val height: Int = belts.map(_.height).sum
}

sealed trait Belt {
  val height: Int = Belt.standardHeight
}
object Belt {
  val standardHeight: Int = 500

  case object Backyard   extends Belt
  case object Moon       extends Belt
  case object Sky        extends Belt
  case object EmptySpace extends Belt
  case object Asertoids  extends Belt
}
