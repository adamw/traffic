package pl.softwaremill.traffic

import processing.core._
import spde.core._
import PConstants._
import PApplet._

import Span._

object Main {
  def main(args: Array[String]) { PApplet.main(Array(classOf[Main].getName)) }
}

class Main extends ProxiedApplet {
  val env = new RunnerComponent
          with UIStateComponent
          with GfxComponent
          with ViewDefinitionComponent
          with SpanToPixelsTranslatorComponent
          with UIVehicleComponent {
    val gfx = Main.this

    val viewDefinition = new ViewDefinition {
      val heightSpan = 100.meters
      val widthSpan = 300.meters
      val heightPixels = 200
      val widthPixels = 600
    }

    val initialVehicles = uiVehicle(Vehicle(TypicalCar, 100.meters, 100.meters, null)) :: Nil
  }

  lazy val px = new DrawProxy(this) {
    size(env.viewDefinition.widthPixels, env.viewDefinition.heightPixels)

    frameRate = 20

    def draw {
      env.runner.step
    }
  }
}

trait GfxComponent {
  val gfx: PApplet
}

trait RunnerComponent {
  this: UIStateComponent with GfxComponent =>

  val runner = new Runner

  class Runner {
    def step {
      for (vehicle <- uiState.vehicles) {
        vehicle.draw()
      }
    }
  }
}