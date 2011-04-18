package pl.softwaremill.traffic

object UIModel

trait UIModelComponent {
  this: GfxComponent with SpanToPixelsTranslatorComponent =>

  trait UIModelObject {
    def draw()
  }

  case class UIVehicle(v: Vehicle) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      val (x, y) = spanToPixelsTranslator.translate((v.p.x, v.p.y))
      val (w, h) = spanToPixelsTranslator.translate((v.vs.length, v.vs.width))
      gfx.rect(x, y, w, h)
    }
  }

  case class UIBarrier(b: Barrier) extends UIModelObject {
    def draw() {

    }
  }

  implicit def vehicleToUIVehicle(v: Vehicle) = UIVehicle(v)
  implicit def barrierToUIBarrier(b: Barrier) = UIBarrier(b)

  implicit def modelObjectToUIModelObject(o: ModelObject): UIModelObject = o match {
    case o: Vehicle => o
    case o: Barrier => o
  }
}