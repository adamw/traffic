package pl.softwaremill.traffic

import processing.core._
import spde.core._

import Span._
import Direction._
import org.scala_tools.time.Imports._

object Main {
  def main(args: Array[String]) { PApplet.main(Array(classOf[Main].getName)) }
}

class Main extends ProxiedApplet {
  val env = new RunnerComponent
          with GfxComponent
          with ViewDefinitionComponent
          with SpanToPixelsTranslatorComponent
          with UIVehicleComponent
          with SimulationStateComponent {
    val gfx = Main.this

    val viewDefinition = new ViewDefinition {
      val heightSpan = 100.meters
      val widthSpan = 200.meters
      val heightPixels = 200
      val widthPixels = 600
    }

    val initialVehicles = Vehicle(TypicalCar, 10.meters, 10.meters, 0.degrees, new Speed(60.kilometers, 1.hour)) :: Nil
  }

  lazy val px = new DrawProxy(this) {
    size(env.viewDefinition.widthPixels, env.viewDefinition.heightPixels)
    frameRate = 20f

    var lastDraw = 0L

    def draw() {
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
  this: SimulationStateComponent with GfxComponent with UIVehicleComponent =>

  val runner = new Runner

  class Runner {
    def step(period: Period) {
      for (vehicle <- state.vehicles) {
        vehicle.draw()
      }

      updateState(SimulationState(state.vehicles.map(_.move(period))))
    }
  }
}