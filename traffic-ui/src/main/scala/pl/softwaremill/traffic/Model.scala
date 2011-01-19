package pl.softwaremill.traffic

import org.scala_tools.time.Imports._

import Span._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Model

class Acceleration

case class Speed(meters: Span, seconds: Period)

trait Vehicle {
  val width: Span
  val length: Span
  val acceleration: Acceleration
}

class Car extends Vehicle {
  val width = 1710 millimeters
  val length = 4490 millimeters
  val acceleration = null //2.6 meters perSecondSquared
}