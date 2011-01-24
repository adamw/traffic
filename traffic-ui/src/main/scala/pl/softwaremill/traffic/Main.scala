package pl.softwaremill.traffic

import processing.core._
import spde.core._
import PConstants._
import PApplet._

import Span._
import Direction._
import org.joda.time.Period

object Main {
  def main(args: Array[String]) { PApplet.main(Array(classOf[Main].getName)) }
}

class Main extends ProxiedApplet {
  val env = new RunnerComponent
          with GfxComponent
          with ViewDefinitionComponent
          with SpanToPixelsTranslatorComponent
          with UIVehicleComponent
          with UIStateComponent {
    val gfx = Main.this

    val viewDefinition = new ViewDefinition {
      val heightSpan = 50.meters
      val widthSpan = 100.meters
      val heightPixels = 200
      val widthPixels = 600
    }

    val initialVehicles = uiVehicle(Vehicle(TypicalCar, 10.meters, 10.meters, 0.degrees, Speed(100.centimeters))) :: Nil
  }

  lazy val px = new DrawProxy(this) {
    size(env.viewDefinition.widthPixels, env.viewDefinition.heightPixels)
    frameRate = 20

    var lastDraw = 0L

    def draw {
      val now = System.currentTimeMillis
      if (lastDraw == 0L) lastDraw = now
      env.runner.step(Period.millis((now-lastDraw).toInt))

      lastDraw = now
    }
  }
}

trait GfxComponent {
  val gfx: PApplet
}

trait RunnerComponent {
  this: UIStateComponent with GfxComponent with UIVehicleComponent =>

  val runner = new Runner

  class Runner {
    def step(period: Period) {
      for (vehicle <- uiState.vehicles) {
        vehicle.draw()
      }

      updateState(UIState(uiState.vehicles.map(_.move(period))))
    }
  }
}