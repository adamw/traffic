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

    trait SimulationObjectsComponentConfigured extends SimulationObjectsComponent {
      this: ViewDefinitionComponent =>

      val static = Lane(Position(10.meters, viewDefinition.heightSpan/2), 90.degrees, 20.meters, 180.meters) :: Nil
    }

    val env = new DrawerComponent
            with MouseEventsComponent
            with GfxComponentConfigured
            with ViewDefinitionComponentConfigured
            with UIModelComponent
            with SimulationObjectsComponentConfigured
            with ToScaleComponent

    val lane1 = env.static(0)

    env.updateDynamic(env.DynamicSimulationObjects(
      Vehicle(TypicalCar, Position(10.meters, 10.meters), 90.degrees, Speed(60.kilometers, 1.hour)) ::
              Vehicle(TypicalCar, Position(10.meters, 20.meters), 90.degrees, Speed(30.kilometers, 1.hour)) :: Nil,
      Barrier(lane1, 40.meters, Barrier.Green) :: Nil))

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
  this: SimulationObjectsComponent with GfxComponent with UIModelComponent with ToScaleComponent =>

  val drawer = new Drawer

  class Drawer {
    def step(period: Period) {
      gfx.background(255);

      toScale {
        for (lane <- static) {
          lane.draw()
        }

        for (modelObject <- dynamic.objects) {
          modelObject.draw()
        }
      }

      updateDynamic(DynamicSimulationObjects(dynamic.vehicles.map(_.move(period)), dynamic.barriers))
    }
  }
}

trait MouseEventsComponent {
  this: SimulationObjectsComponent with UIModelComponent =>

  val mouseEvents = new MouseEvents

  class MouseEvents {
    def mouseClicked(x: Int, y: Int ) {
      updateDynamic(DynamicSimulationObjects(dynamic.vehicles, dynamic.barriers.map(_.switch)))
    }
  }
}