module Model where

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }

type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float, aMss: Float }

data TLState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight

-- work-around for https://github.com/evancz/Elm/issues/294
data TLUpdateFn = TLUpdateFn { fn: (TLState, Time) -> (TLState, TLUpdateFn) }
noOpTLUpdateFn = TLUpdateFn { fn = \(state, t) -> (state, noOpTLUpdateFn) }

type TLSwitchTimings = { yellowAfterRed: Float,
                         yellowAfterGreen: Float }
type TrafficLight = { posM: PosM, 
                      direction: Float,
                      state: TLState, 
                      updateFn: TLUpdateFn,
                      switchTimings: TLSwitchTimings }

type CarCreator = { posM: PosM, direction: Float }

type Annihilator = { minX: Float, maxX: Float, minY: Float, maxY: Float }

data Obj = CarObj Car 
         | TrafficLightObj TrafficLight
         | CarCreatorObj CarCreator

posMOfObj: Obj -> PosM
posMOfObj obj =
  case obj of
    CarObj car -> car.posM
    TrafficLightObj trafficLight -> trafficLight.posM
    CarCreatorObj carCreator -> carCreator.posM
    _ -> { xM = 0, yM = 0 }

speedKphOfObj: Obj -> Float
speedKphOfObj obj =
  case obj of
    CarObj car -> car.speedKph
    _ -> 0

speedKphToMps: Float -> Float
speedKphToMps speedKph = speedKph * 5 / 18 -- 1000/3600

distM: PosM -> PosM -> Float
distM p1 p2 = sqrt ((p1.xM - p2.xM)^2 + (p1.yM - p2.yM)^2)

isTrafficLight: Obj -> Bool
isTrafficLight obj = case obj of { TrafficLightObj tl -> True ; _ -> False }

-- CONSTANTS
oneSecond = 1000