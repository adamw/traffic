package pl.softwaremill.traffic

object View

trait ViewDefinitionComponent {
  val viewDefinition: ViewDefinition

  trait ViewDefinition {
    val widthPixels: Int
    val heightPixels: Int
    val widthSpan: Span
    val heightSpan: Span
  }
}

trait PositionToPixelsTranslatorComponent {
  this: ViewDefinitionComponent =>

  val positionToPixelsTranslator = new PositionToPixelsTranslator

  class PositionToPixelsTranslator {
    def translate(pos: Position): (Int, Int) = {
      val posX = translateToScale(pos.x.mm, viewDefinition.widthSpan.mm, viewDefinition.widthPixels)
      val posY = translateToScale(pos.y.mm, viewDefinition.heightSpan.mm, viewDefinition.heightPixels)
      (posX, posY)
    }

    private def translateToScale(current: Long, max: Long, maxScaled: Int): Int = {
      ((current.toDouble / max.toDouble) * maxScaled.toDouble).toInt
    }
  }
}