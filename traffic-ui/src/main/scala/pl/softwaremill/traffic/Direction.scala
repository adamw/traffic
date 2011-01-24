package pl.softwaremill.traffic

import scala.math.toRadians

case class Direction(degrees: Double) {
  def cos = scala.math.cos(toRadians(degrees))
  def sin = scala.math.sin(toRadians(degrees))
}

object Direction {
  class RichInt(n: Int) {
    def degrees = new Direction(n)
  }

  implicit def intToRichInt(n: Int) = new RichInt(n)
}