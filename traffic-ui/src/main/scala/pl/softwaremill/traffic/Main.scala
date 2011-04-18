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
  trait ViewDefinitionComponentConfigured extends ViewDefinitionComponent {
    val viewDefinition = new ViewDefinition {
      val heightSpan = 100.meters
      val widthSpan = 200.meters
      val heightPixels = 200
      val widthPixels = 600
    }
  }

  trait GfxComponentConfigured extends GfxComponent {
    val gfx = Main.this
  }

  val env = new RunnerComponent
          with GfxComponentConfigured
          with ViewDefinitionComponentConfigured
          with SpanToPixelsTranslatorComponent
          with UIModelComponent
          with SimulationStateComponent

  env.updateState(env.SimulationState(
    Vehicle(TypicalCar, Position(10.meters, 10.meters), 0.degrees, Speed(60.kilometers, 1.hour)) ::
    Vehicle(TypicalCar, Position(10.meters, 20.meters), 0.degrees, Speed(30.kilometers, 1.hour)) :: Nil,
    Barrier(Position(40.meters, 5.meters), Position(41.meters, 25.meters)) :: Nil))

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
  this: SimulationStateComponent with GfxComponent with UIModelComponent =>

  val runner = new Runner

  class Runner {
    def step(period: Period) {
      gfx.background(100);

      for (modelObject <- state.objects) {
        modelObject.draw()
      }

      updateState(SimulationState(state.vehicles.map(_.move(period)), state.barriers))
    }
  }
}