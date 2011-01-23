package pl.softwaremill.traffic

import org.scala_tools.time.Imports._

import Span._
import Acceleration._

class Model

case class Speed(meters: Span, inTime: Period)

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

case class Vehicle(val vs: VehicleSpecification, val x: Span, val y: Span, val cs: Speed) {
}