package pl.softwaremill.traffic

import org.joda.time._

import Span._
import Acceleration._

object Model

sealed abstract class ModelObject()

trait VehicleSpecification {
  val width: Span
  val length: Span
  val acceleration: Acceleration
}

object TypicalCar extends VehicleSpecification {
  val width = 1710 millimeters
  val length = 4490 millimeters
  val acceleration = 2.6.meters.perSecondSquared
}

case class Vehicle(vs: VehicleSpecification, p: Position, direction: Direction, currentSpeed: Speed) extends ModelObject {
  def move(period: Period) = {
    val distance = currentSpeed * period
    val xDistance = distance * direction.cos
    val yDistance = distance * direction.sin
    Vehicle(vs, p + (xDistance, yDistance), direction, currentSpeed)
  }
}

case class Barrier(topLeft: Position, bottomRight: Position, state: Barrier.State) extends ModelObject {
  def switch = Barrier(topLeft, bottomRight, state.switch)
}

object Barrier {
  sealed abstract class State() {
    def switch = this match {
      case Red => Green
      case Green => Red
    }
  }

  case object Red extends State
  case object Green extends State
}