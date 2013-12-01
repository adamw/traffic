module Model where

import Random.Generator
import Random.Standard

type ClusterId = Int

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: SizeM, centerM: PosM }

type Positioned a = { a | posM: PosM }
type Clustered a = { a | clusterId: ClusterId }
type Directed a = { a | direction: Float }
type ObjParams a = Positioned (Clustered (Directed a)) 

type Happiness = { raw: Int, val: Int, nextUpdate: Float }
type Car = ObjParams { speedKph: Float, sizeM: SizeM, aMss: Float, happiness: Happiness }

type TLId = Int
data TLState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight
type TrafficLight = ObjParams { tlId: TLId,
                                state: TLState }

type CarCreator = ObjParams {}

type Annihilator = { minX: Float, maxX: Float, minY: Float, maxY: Float }

data TLCtrlStep = SwitchStep TLState TLId
                | WaitStep Time
                | SetAutoInt TLAutoInt
type TLAutoInt = { lr: Int, td: Int }
type TrafficLightsCtrl = {
  groupLR: [ TLId ],
  groupTD: [ TLId ],
  yellowAfterRed: Float,
  yellowAfterGreen: Float,
  betweenRed: Float,
  steps: [ TLCtrlStep ],
  renewSteps: Maybe [ TLCtrlStep ] 
}

data Obj = Car Car 
         | TrafficLight TrafficLight
         | CarCreator CarCreator

type ObjWithDist = { obj: Obj, distMToPrev: Float }

type RandStdGen = Random.Generator.Generator Random.Standard.Standard

type World = { objs: [ Obj ], 
               ann: Annihilator,
               tlCtrl: TrafficLightsCtrl,
               random: RandStdGen }

posMOfObj: Obj -> PosM
posMOfObj obj =
  case obj of
    Car car -> car.posM
    TrafficLight trafficLight -> trafficLight.posM
    CarCreator carCreator -> carCreator.posM
    _ -> { xM = 0, yM = 0 }

speedKphOfObj: Obj -> Float
speedKphOfObj obj =
  case obj of
    Car car -> car.speedKph
    _ -> 0

directionOfObj: Obj -> Float
directionOfObj obj =
  case obj of
    Car car -> car.direction
    TrafficLight trafficLight -> trafficLight.direction
    CarCreator carCreator -> carCreator.direction
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
isTrafficLight obj = case obj of { TrafficLight tl -> True ; _ -> False }

clusterIdOfObj: Obj -> Int
clusterIdOfObj obj = 
  case obj of
    Car car -> car.clusterId
    TrafficLight trafficLight -> trafficLight.clusterId
    CarCreator carCreator -> carCreator.clusterId
    _ -> -1

-- CONSTANTS
oneSecond = 1000