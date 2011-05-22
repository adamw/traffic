package pl.softwaremill.traffic

trait AddToLaneComponent {
  this: SimulationObjectsComponent =>
  implicit def vehicleSpecificationToAdder(vs: VehicleSpecification) = new Adder(vs)

  class Adder(vs: VehicleSpecification) {
    def withSpeed(s: Speed) = new AdderWithSpeed(vs, s);
  }

  class AdderWithSpeed(vs: VehicleSpecification, s: Speed) {
    def addToLane(l: Lane) {
      dynamicObjects.addVehicle(Vehicle(vs, l.axisStart, l.direction, s))
    }
  }
}