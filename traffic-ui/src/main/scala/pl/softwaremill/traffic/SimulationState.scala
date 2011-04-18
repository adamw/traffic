package pl.softwaremill.traffic

object SimulationState

trait SimulationStateComponent {
  def state = currentState
  def updateState(state: SimulationState) {
    currentState = state
  }

  case class SimulationState(vehicles: List[Vehicle], barriers: List[Barrier]) {
    def objects: List[ModelObject] = vehicles ++ barriers
  }

  private var currentState = SimulationState(Nil, Nil)
}

