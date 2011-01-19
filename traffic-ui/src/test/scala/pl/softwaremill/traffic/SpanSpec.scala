package pl.softwaremill.traffic

import org.specs._
import Span._

class SpanSpec extends SpecificationWithJUnit {
  "Conversion from int" should {
    "work for millimeters" in {
      (13 millimeters) must_== Span(13)
    }

    "work for centimeters" in {
      (5 centimeters) must_== Span(50)
    }

    "work for meters" in {
      (812 meters) must_== Span(812000)
    }

    "work for kilometers" in {
      (2 kilometers) must_== Span(2000000)
    }
  }

  "Same length in different units" should {
    "should be equal for meters and centimeters" in {
      (2 meters) must_== (200 centimeters)
    }

    "should be equal for kilometers and meters" in {
      (15 kilometers) must_== (15000 meters)
    }
  }

  "Conversion from double" should {
    "work for millimeters" in {
      (51.5 millimeters) must_== Span(51)
    }

    "work for centimeters" in {
      (12.4 centimeters) must_== Span(124)
    }

    "work for meters" in {
      (87.9123 meters) must_== Span(87912)
    }

    "work for kilometers" in {
      (2.9 kilometers) must_== Span(2900000)
    }
  }
}