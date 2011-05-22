package pl.softwaremill.traffic

trait SimulationObjectsComponent {
  val staticObjects: List[Lane]

  val dynamicObjects = new DynamicSimulationObjects

  class DynamicSimulationObjects extends Iterable[ModelObject] {
    var vehicles: List[Vehicle] = Nil
    var barriers: List[Barrier] = Nil

    def addVehicle(v: Vehicle) {
      vehicles = v :: vehicles
    }

    def iterator = (vehicles ++ barriers).iterator
  }
}

