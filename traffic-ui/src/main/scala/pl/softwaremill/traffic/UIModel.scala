package pl.softwaremill.traffic

import processing.core.PConstants

import Span._

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

  private val barrierWidthOffset = 1.meters
  private val barrierHeight = 50.centimeters

  case class UIBarrier(b: Barrier) extends UIModelObject {
    def draw() {
      gfx.stroke(0)
      gfx.fill(0)

      val color = b.state match {
        case Barrier.Red => (255, 0, 0)
        case Barrier.Green => (0, 255, 0)
      }

      gfx.pushMatrix()

      laneRelative(b.lane)

      gfx.rect(- (b.lane.width/2 + barrierWidthOffset), - (b.fromStart), b.lane.width + barrierWidthOffset*2, barrierHeight)

      val radius = 1.meters

      gfx.fill(color._1, color._2, color._3)
      gfx.ellipseMode(PConstants.RADIUS)
      gfx.ellipse(0, - (b.fromStart - barrierHeight/2), radius, radius)

      gfx.popMatrix()
    }
  }

  case class UILane(l: Lane) extends UIModelObject {
    def draw() {
      gfx.stroke(128)
      gfx.fill(128)

      val height = l.length
      val halfWidth = l.width/2

      gfx.pushMatrix()
      laneRelative(l)
      gfx.beginShape(PConstants.QUAD)
      gfx.vertex(- halfWidth, 0)
      gfx.vertex(halfWidth, 0)
      gfx.vertex(halfWidth, - height)
      gfx.vertex(- halfWidth, - height)
      gfx.endShape()
      gfx.popMatrix()
    }
  }

  private def laneRelative(l: Lane) {
    gfx.translate(l.axisStart.x, l.axisStart.y)
    gfx.rotate(l.direction.radians.toFloat)
  }

  implicit def vehicleToUIVehicle(v: Vehicle) = UIVehicle(v)
  implicit def barrierToUIBarrier(b: Barrier) = UIBarrier(b)
  implicit def laneToUILane(l: Lane) = UILane(l)

  implicit def modelObjectToUIModelObject(o: ModelObject): UIModelObject = o match {
    case o: Vehicle => o
    case o: Barrier => o
  }
}