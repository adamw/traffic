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
  type UIComponents = DrawerComponent with MouseEventsComponent with ViewDefinitionComponent

  def createEnv: UIComponents = {
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

    val env = new DrawerComponent
            with MouseEventsComponent
            with GfxComponentConfigured
            with ViewDefinitionComponentConfigured
            with SpanToPixelsTranslatorComponent
            with UIModelComponent
            with SimulationStateComponent

    env.updateState(env.SimulationState(
      Vehicle(TypicalCar, Position(10.meters, 10.meters), 0.degrees, Speed(60.kilometers, 1.hour)) ::
              Vehicle(TypicalCar, Position(10.meters, 20.meters), 0.degrees, Speed(30.kilometers, 1.hour)) :: Nil,
      Barrier(Position(40.meters, 5.meters), Position(41.meters, 25.meters), Barrier.Green) :: Nil))

    env
  }

  def createDrawProxy(env: UIComponents) = {
    new DrawProxy(this) {
      size(env.viewDefinition.widthPixels, env.viewDefinition.heightPixels)
      frameRate = 20f

      var lastDraw = 0L

      def draw() {
        val now = System.currentTimeMillis
        if (lastDraw == 0L) lastDraw = now
        env.drawer.step(Period.millis((now-lastDraw).toInt))

        lastDraw = now
      }

      override def mouseClicked() {
        env.mouseEvents.mouseClicked(mouseX, mouseY)
      }
    }
  }

  lazy val px = createDrawProxy(createEnv)
}

trait GfxComponent {
  val gfx: PApplet
}

trait DrawerComponent {
  this: SimulationStateComponent with GfxComponent with UIModelComponent =>

  val drawer = new Drawer

  class Drawer {
    def step(period: Period) {
      gfx.background(255);

      for (modelObject <- state.objects) {
        modelObject.draw()
      }

      updateState(SimulationState(state.vehicles.map(_.move(period)), state.barriers))
    }
  }
}

trait MouseEventsComponent {
  this: SimulationStateComponent with UIModelComponent =>

  val mouseEvents = new MouseEvents

  class MouseEvents {
    def mouseClicked(x: Int, y: Int ) {
      updateState(SimulationState(state.vehicles, state.barriers.map(_.switch)))
    }
  }
}