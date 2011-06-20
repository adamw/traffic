package pl.softwaremill.traffic

import org.specs2.mutable._
import Span._
import Acceleration._

class AccelerationSpec extends SpecificationWithJUnit {
  "Conversion from spans " should {
    "work for meters" in {
      9.12.meters.perSecondSquared must_== Acceleration(9.12)
    }
  }
}