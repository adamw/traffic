package pl.softwaremill.traffic

import org.scala_tools.time.Imports._

import Span._
import Acceleration._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Model

case class Speed(meters: Span, seconds: Period)

trait Vehicle {
  val width: Span
  val length: Span
  val acceleration: Acceleration
}

class Car extends Vehicle {
  val width = 1710 millimeters
  val length = 4490 millimeters
  val acceleration = 2.6.meters.perSecondSquared
}