package pl.softwaremill.traffic

case class Acceleration(metersPerSecondSquared: Double)

object Acceleration {
  class RichSpan(span: Span) {
    def perSecondSquared = Acceleration(span.mm.toDouble/1000)
  }

  implicit def spanToRichSpan(span: Span) = new RichSpan(span)
}