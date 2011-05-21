package pl.softwaremill.traffic

import scala.math.toRadians
import org.joda.time._

object Physics

case class Span(mm: Long) {
  def +(other: Span) = Span(mm + other.mm)
  def *(by: Double) = Span((mm.toDouble * by).toLong)
  def /(by: Int) = Span(mm / by)
  def /(by: Long) = Span(mm / by)
}

object Span {
  class RichInt(n: Int) {
    def millimeters = new Span(n)
    def centimeters = new Span(n*10)
    def meters = new Span(n*1000)
    def kilometers = new Span(n*1000*1000)
  }

  class RichDouble(d: Double) {
    def millimeters = new Span(d.toInt)
    def centimeters = new Span((d*10).toInt)
    def meters = new Span((d*1000).toInt)
    def kilometers = new Span((d*1000*1000).toInt)
  }

  implicit def intToRichInt(n: Int) = new RichInt(n)
  implicit def doubleToRichDouble(d: Double) = new RichDouble(d)

  implicit def spanToFloat(span: Span) = span.mm.toFloat
}

case class Acceleration(metersPerSecondSquared: Double)

object Acceleration {
  class RichSpan(span: Span) {
    def perSecondSquared = Acceleration(span.mm.toDouble/1000)
  }

  implicit def spanToRichSpan(span: Span) = new RichSpan(span)
}

case class Direction(degrees: Double) {
  val radians = toRadians(degrees)

  def cos = scala.math.cos(radians)
  def sin = scala.math.sin(radians)
}

object Direction {
  class RichInt(n: Int) {
    def degrees = new Direction(n)
  }

  implicit def intToRichInt(n: Int) = new RichInt(n)
}

case class Speed(metersPerSecond: Span) {
  def *(period: Period): Span = {
    metersPerSecond * (period.toDurationFrom(new DateTime).getMillis.toDouble / 1000)
  }
}

object Speed {
  def apply(meters: Span, perPeriod: Period): Speed = {
    Speed(meters / perPeriod.toDurationFrom(new DateTime).getStandardSeconds)
  }
}

case class Position(x: Span, y: Span) {
  def +(other: Position) = Position(x + other.x, y + other.y)
  def +(other: (Span, Span)): Position = this.+(Position(other._1, other._2))
}

object Position {
  implicit def spansToPosition(pair: (Span, Span)) = Position(pair._1, pair._2)
}