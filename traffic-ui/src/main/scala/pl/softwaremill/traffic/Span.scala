package pl.softwaremill.traffic

case class Span(mm: Long) {
  def +(other: Span) = Span(mm + other.mm)
  def *(by: Double) = Span((mm.toDouble * by).toLong)
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
}