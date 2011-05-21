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

trait ToScaleComponent {
  this: ViewDefinitionComponent with GfxComponent =>

  val toScale = new ToScale

  class ToScale {
    def apply(block: => Unit) {
      gfx.scale(viewDefinition.widthPixels.toFloat / viewDefinition.widthSpan.mm.toFloat,
        viewDefinition.heightPixels.toFloat / viewDefinition.heightSpan.mm.toFloat)
      gfx.pushMatrix()

      block

      gfx.popMatrix()
    }
  }
}