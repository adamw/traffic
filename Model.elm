module Model where

type ClusterId = Int

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: SizeM, centerM: PosM }

type Obj a = { a | posM: PosM, clusterId: ClusterId, direction: Float }
type MovingObj a = Obj { a | speedKph: Float }
type AnyObj = Obj {}

type Car = MovingObj { sizeM: SizeM, aMss: Float }

type TLId = Int
data TLState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight
type TrafficLight = Obj { tlId: TLId, state: TLState }

type CarCreator = Obj {}

type Annihilator = { minX: Float, maxX: Float, minY: Float, maxY: Float }

data TLCtrlStep = SwitchStep TLState TLId
                | WaitStep Time
type TrafficLightsCtrl = {
  groupA: [ TLId ],
  groupB: [ TLId ],
  yellowAfterRed: Float,
  yellowAfterGreen: Float,
  betweenRed: Float,
  steps: [ TLCtrlStep ]
}

type MaybeCarWithDist = { car: Maybe Car, obj: AnyObj, distMToPrev: Float }

type World = { cars: [ Car ], 
               carCreators: [ CarCreator ],
               tls: [ TrafficLight ],
               ann: Annihilator,
               tlCtrl: TrafficLightsCtrl }

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

-- CONSTANTS
oneSecond = 1000