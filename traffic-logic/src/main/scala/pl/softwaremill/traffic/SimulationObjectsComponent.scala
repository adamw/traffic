package pl.softwaremill.traffic

trait SimulationObjectsComponent {
  val static: List[Lane]

  val dynamic = new DynamicSimulationObjects

  class DynamicSimulationObjects {
    case class State(vehicles: List[Vehicle], barriers: List[Barrier])

    private var currentState = State(Nil, Nil)

    def vehicles = currentState.vehicles
    def barriers = currentState.barriers

    def objects: List[ModelObject] = currentState.vehicles ++ currentState.barriers

    def addVehicle(v: Vehicle) {
      currentState = State(v :: currentState.vehicles, currentState.barriers)
    }

    def updateVehicles(vehicles: List[Vehicle]) {
      currentState = currentState.copy(vehicles = vehicles)
    }

    def updateBarriers(barriers: List[Barrier]) {
      currentState = currentState.copy(barriers = barriers)
    }
  }
}

