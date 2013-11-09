module Model where

type ClusterId = Int

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: SizeM, centerM: PosM }

type Positioned a = { a | posM: PosM }
type Clustered a = { a | clusterId: ClusterId }
type Directed a = { a | direction: Float }
type ObjParams a = Positioned (Clustered (Directed a)) 

type Car = ObjParams { speedKph: Float, sizeM: SizeM, aMss: Float }

data TLState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight

-- work-around for https://github.com/evancz/Elm/issues/294
data TLUpdateFn = TLUpdateFn { fn: (TLState, Time) -> (TLState, TLUpdateFn) }
noOpTLUpdateFn = TLUpdateFn { fn = \(state, t) -> (state, noOpTLUpdateFn) }

type TLSwitchTimings = { yellowAfterRed: Float,
                         yellowAfterGreen: Float }
type TrafficLight = ObjParams { state: TLState, 
                                updateFn: TLUpdateFn,
                                switchTimings: TLSwitchTimings }

type CarCreator = ObjParams {}

type Annihilator = { minX: Float, maxX: Float, minY: Float, maxY: Float }

data Obj = CarObj Car 
         | TrafficLightObj TrafficLight
         | CarCreatorObj CarCreator

type ObjWithDist = { obj: Obj, distMToPrev: Float }

type World = { objs: [ Obj ], ann: Annihilator }

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

directionOfObj: Obj -> Float
directionOfObj obj =
  case obj of
    CarObj car -> car.direction
    TrafficLightObj trafficLight -> trafficLight.direction
    CarCreatorObj carCreator -> carCreator.direction
    _ -> degrees 0

speedKphToMps: Float -> Float
speedKphToMps speedKph = speedKph * 5 / 18 -- 1000/3600

-- Due to rounding errors we consider coordinates that are very close to each other equal
coordEq: Float -> Float -> Bool
coordEq a b = (abs (a - b)) < 0.000001

distM: PosM -> PosM -> Float
distM p1 p2 = 
  if (coordEq p1.xM p2.xM) then (abs (p1.yM - p2.yM)) else
    if (coordEq p1.yM p2.yM) then (abs (p1.xM - p2.xM)) else
      sqrt ((p1.xM - p2.xM)^2 + (p1.yM - p2.yM)^2)

isTrafficLight: Obj -> Bool
isTrafficLight obj = case obj of { TrafficLightObj tl -> True ; _ -> False }

clusterIdOfObj: Obj -> Int
clusterIdOfObj obj = 
  case obj of
    CarObj car -> car.clusterId
    TrafficLightObj trafficLight -> trafficLight.clusterId
    CarCreatorObj carCreator -> carCreator.clusterId
    _ -> -1

-- CONSTANTS
oneSecond = 1000