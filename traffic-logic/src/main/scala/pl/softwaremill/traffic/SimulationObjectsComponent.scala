package pl.softwaremill.traffic

trait SimulationObjectsComponent {
  val staticObjects: List[Lane]

  val dynamicObjects = new DynamicSimulationObjects

  class DynamicSimulationObjects extends Iterable[ModelObject] {
    case class State(vehicles: List[Vehicle], barriers: List[Barrier])

    private var currentState = State(Nil, Nil)

    def vehicles = currentState.vehicles
    def barriers = currentState.barriers

    def addVehicle(v: Vehicle) {
      currentState = State(v :: currentState.vehicles, currentState.barriers)
    }

    def updateVehicles(vehicles: List[Vehicle]) {
      currentState = currentState.copy(vehicles = vehicles)
    }

    def updateBarriers(barriers: List[Barrier]) {
      currentState = currentState.copy(barriers = barriers)
    }

    def iterator = (currentState.vehicles ++ currentState.barriers).iterator
  }
}

