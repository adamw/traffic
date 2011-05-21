package pl.softwaremill.traffic

import processing.core.PConstants

object UIModel

trait UIModelComponent {
  this: GfxComponent with ViewDefinitionComponent =>

  trait UIModelObject {
    def draw()
  }

  case class UIVehicle(v: Vehicle) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(255)
      gfx.rect(v.p.x, v.p.y, v.vs.length, v.vs.width)
    }
  }

  case class UIBarrier(b: Barrier) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(0)
      val (x1, y1) = (b.topLeft.x, b.topLeft.y)
      val (x2, y2) = (b.bottomRight.x, b.bottomRight.y)
      gfx.rect(x1, y1, x2-x1, y2-y1)

      val color = b.state match {
        case Barrier.Red => (255, 0, 0)
        case Barrier.Green => (0, 255, 0)
      }

      val radius = (x2-x1).abs * 2

      gfx.fill(color._1, color._2, color._3)
      gfx.ellipseMode(PConstants.RADIUS)
      gfx.ellipse((x1+x2)/2, (y1+y2)/2, radius, radius)
    }
  }

  case class UILane(l: Lane) extends UIModelObject {
    def draw() {
      gfx.stroke(128)
      gfx.fill(128)

      val height = l.length
      val halfWidth = l.width/2

      gfx.pushMatrix()
      gfx.translate(l.axisStart.x, l.axisStart.y)
      gfx.rotate(l.direction.radians.toFloat)
      gfx.beginShape(PConstants.QUAD)
      gfx.vertex(- halfWidth, 0)
      gfx.vertex(halfWidth, 0)
      gfx.vertex(halfWidth, - height)
      gfx.vertex(- halfWidth, - height)
      gfx.endShape()
      gfx.popMatrix()
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