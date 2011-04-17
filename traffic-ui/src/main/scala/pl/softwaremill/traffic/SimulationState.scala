package pl.softwaremill.traffic

object SimulationState

trait SimulationStateComponent {
  val initialVehicles: List[Vehicle]

  def state = currentState()
  def updateState(state: SimulationState) {
    currentState = () => state
  }

  case class SimulationState(vehicles: List[Vehicle])

  private var currentState = () => SimulationState(initialVehicles)
}