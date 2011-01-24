package pl.softwaremill.traffic

object UIState

trait UIStateComponent {
  this: UIVehicleComponent =>

  val initialVehicles: List[UIVehicle]

  def uiState = currentState()
  def updateState(state: UIState) {
    currentState = () => state
  }

  case class UIState(vehicles: List[UIVehicle])

  private var currentState = () => UIState(initialVehicles)
}