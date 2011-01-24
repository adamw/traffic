package pl.softwaremill.traffic

object UIModel

trait UIVehicleComponent {
  this: GfxComponent with SpanToPixelsTranslatorComponent =>

  def uiVehicle(v: Vehicle) = { UIVehicle(v) };

  case class UIVehicle(v: Vehicle) {
    def draw() {
      gfx.stroke(0)
      val (x, y) = spanToPixelsTranslator.translate((v.x, v.y))
      val (w, h) = spanToPixelsTranslator.translate((v.vs.length, v.vs.width))
      gfx.rect(x, y, w, h)
    }
  }
}
