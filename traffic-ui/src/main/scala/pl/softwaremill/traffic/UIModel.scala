package pl.softwaremill.traffic

class UIModel

trait UIVehicleComponent {
  this: GfxComponent with SpanToPixelsTranslatorComponent =>

  def uiVehicle(v: Vehicle) = { UIVehicle(v) };

  case class UIVehicle(v: Vehicle) {
    def draw() {
      gfx.stroke(0)
      val (x, y) = spanToPixelsTranslator.translate((v.x, v.y))
      val (w, h) = spanToPixelsTranslator.translate((v.vs.length, v.vs.width))
      gfx.rect(x, y, w, h)
    }
  }
}

trait ViewDefinitionComponent {
  val viewDefinition: ViewDefinition

  trait ViewDefinition {
    val widthPixels: Int
    val heightPixels: Int
    val widthSpan: Span
    val heightSpan: Span
  }
}

trait SpanToPixelsTranslatorComponent {
  this: ViewDefinitionComponent =>

  val spanToPixelsTranslator = new SpanToPixelsTranslator

  class SpanToPixelsTranslator {
    def translate(pos: (Span, Span)): (Int, Int) = {
      val posX = translateToScale(pos._1.mm, viewDefinition.widthSpan.mm, viewDefinition.widthPixels)
      val posY = translateToScale(pos._2.mm, viewDefinition.heightSpan.mm, viewDefinition.heightPixels)
      (posX, posY)
    }

    private def translateToScale(current: Long, max: Long, maxScaled: Int): Int = {
      ((current.toDouble / max.toDouble) * maxScaled.toDouble).toInt
    }
  }
}

trait UIStateComponent {
  this: UIVehicleComponent =>

  val initialVehicles: List[UIVehicle]

  def uiState = currentState

  case class UIState(vehicles: List[UIVehicle])

  // TODO
  private def currentState: UIState = UIState(initialVehicles)
}