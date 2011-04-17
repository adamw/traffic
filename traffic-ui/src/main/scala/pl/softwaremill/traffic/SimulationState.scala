package pl.softwaremill.traffic

object SimulationState

trait SimulationStateComponent {
  this: InitialStateComponent =>

  def state = currentState
  def updateState(state: SimulationState) {
    currentState = state
  }

  case class SimulationState(vehicles: List[Vehicle])

  private var currentState = SimulationState(initialVehicles)
}

trait InitialStateComponent {
  val initialVehicles: List[Vehicle]
}