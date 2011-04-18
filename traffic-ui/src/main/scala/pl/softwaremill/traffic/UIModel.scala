package pl.softwaremill.traffic

object UIModel

trait UIModelComponent {
  this: GfxComponent with SpanToPixelsTranslatorComponent =>

  case class UIVehicle(v: Vehicle) {
    def draw() {
      gfx.stroke(0)
      val (x, y) = spanToPixelsTranslator.translate((v.p.x, v.p.y))
      val (w, h) = spanToPixelsTranslator.translate((v.vs.length, v.vs.width))
      gfx.rect(x, y, w, h)
    }
  }

  implicit def vehicleToUIVehicle(v: Vehicle) = UIVehicle(v)

  case class UIBarrier(b: Barrier) {
    def draw() {

    }
  }

  implicit def barrierToUIBarrier(b: Barrier) = UIBarrier(b)
}