package pl.softwaremill.traffic

import processing.core.PConstants

object UIModel

trait UIModelComponent {
  this: GfxComponent with SpanToPixelsTranslatorComponent =>

  trait UIModelObject {
    def draw()
  }

  case class UIVehicle(v: Vehicle) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(255)
      val (x, y) = spanToPixelsTranslator.translate((v.p.x, v.p.y))
      val (w, h) = spanToPixelsTranslator.translate((v.vs.length, v.vs.width))
      gfx.rect(x, y, w, h)
    }
  }

  case class UIBarrier(b: Barrier) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(0)
      val (x1, y1) = spanToPixelsTranslator.translate(b.topLeft)
      val (x2, y2) = spanToPixelsTranslator.translate(b.bottomRight)
      gfx.rect(x1, y1, x2-x1, y2-y1)

      val color = b.state match {
        case Barrier.Red => (255, 0, 0)
        case Barrier.Green => (0, 255, 0)
      }

      gfx.fill(color._1, color._2, color._3)
      gfx.ellipseMode(PConstants.RADIUS)
      gfx.ellipse((x1+x2)/2, (y1+y2)/2, 4, 4)
    }
  }

  case class UILane(l: Lane) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(0)

    }
  }

  implicit def vehicleToUIVehicle(v: Vehicle) = UIVehicle(v)
  implicit def barrierToUIBarrier(b: Barrier) = UIBarrier(b)
  implicit def laneToUILane(l: Lane) = UILane(l)

  implicit def modelObjectToUIModelObject(o: ModelObject): UIModelObject = o match {
    case o: Vehicle => o
    case o: Barrier => o
  }
}