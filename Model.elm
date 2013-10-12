module Model where

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }

type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float, aMss: Float }

data TrafficLightState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight
type TrafficLight = { posM: PosM, state: TrafficLightState, direction: Float }

data Obj = CarObj Car | TrafficLightObj TrafficLight

posMOfObj: Obj -> PosM
posMOfObj obj =
  case obj of
    CarObj car -> car.posM
    TrafficLightObj trafficLight -> trafficLight.posM

speedKphOfObj: Obj -> Float
speedKphOfObj obj =
  case obj of
    CarObj car -> car.speedKph
    _ -> 0

speedKphToMps: Float -> Float
speedKphToMps speedKph = speedKph * 1000 / 3600