package pl.softwaremill.traffic

trait SimulationObjectsComponent {
  val static: List[Lane]

  def dynamic = currentDynamicObjects
  def updateDynamic(state: DynamicSimulationObjects) {
    currentDynamicObjects = state
  }

  case class DynamicSimulationObjects(vehicles: List[Vehicle], barriers: List[Barrier]) {
    def objects: List[ModelObject] = vehicles ++ barriers
  }

  private var currentDynamicObjects = DynamicSimulationObjects(Nil, Nil)
}

