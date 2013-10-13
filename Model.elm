module Model where

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }

type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float, aMss: Float }

data TLState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight

-- work-around for https://github.com/evancz/Elm/issues/215
data TLUpdateFn = TLUpdateFn { fn: (TLState, Time) -> (TLState, TLUpdateFn) }
noOpTLUpdateFn = TLUpdateFn { fn = \(state, t) -> (state, noOpTLUpdateFn) }

type TLSwitchTimings = { yellowAfterRed: Float,
                         yellowAfterGreen: Float }
type TrafficLight = { posM: PosM, 
                      direction: Float,
                      state: TLState, 
                      updateFn: TLUpdateFn,
                      switchTimings: TLSwitchTimings }

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

-- CONSTANTS
oneSecond = 1000
halfSecond = oneSecond/2