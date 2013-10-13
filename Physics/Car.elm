module Physics.Car(drive) where

import open Model
import NearestObstacle

-- UPDATE

{-
1. detect what's ahead - find the closest object
2. decide if we need to start slowing down
   - barrier - always if close enough
   - another car - if close enough and moving slower
3. if so, start slowing down
4. if we can accelerate, accelerate
   - only if the next car is far away enough

deceleration - maximum? how far ahead we are going to start stopping

yellow light: should be on long enough for a car with a distance < comfortable breaking
distance to pass
-}

-- interesting ref: 
-- http://cs.zstu.edu.cn/udpaloolapdu/Files/20101122180945.pdf
-- http://www.direct.gov.uk/prod_consum_dg/groups/dg_digitalassets/@dg/@en/@motor/documents/digitalasset/dg_188029.pdf

-- see http://en.wikipedia.org/wiki/Braking_distance#Total_stopping_distance
slowReactionTimeS = 2.5
fastReactionTimeS = 1.5

gAccelMss = 9.81
comfortableDecelMss = gAccelMss * 0.35
maxDecelMss = gAccelMss * 0.7

type ObjAheadParams = { speedKph: Float, minSeparationM: Float, distanceM: Float }

{--
s = att/2
v = at -> t = v/a

s = a(v/a)(v/a)/2 = vv/2a -> a = vv/2s
--}

{--
What is the desireable distance between two objects, if we can break with the 
given deceleration?
--}
desiredDistanceM: Car -> ObjAheadParams -> Float -> Float
desiredDistanceM car otherParams decelMss =
  let decisionDistanceM = slowReactionTimeS * (speedKphToMps car.speedKph)
      extraSafetyDistanceM = 0 -- TODO
      minSeparationM = otherParams.minSeparationM + car.sizeM.lengthM / 2
      -- TODO real min separation is 2 + obj_len/2
  in  max (decisionDistanceM + extraSafetyDistanceM) minSeparationM

computeAccel: Float -> Float -> Float
computeAccel approachSpeedKph deltaDistanceM =
  let a = (speedKphToMps approachSpeedKph)^2/(2*deltaDistanceM) 
  in  if | a < comfortableDecelMss -> 0
         | a > maxDecelMss -> -maxDecelMss
         | otherwise -> -a

accelForCarGivenAhead: Car -> ObjAheadParams -> Float
accelForCarGivenAhead car otherParams =
  let approachSpeedKph = car.speedKph - otherParams.speedKph
      safeDistanceM = desiredDistanceM car otherParams comfortableDecelMss
      deltaDistanceM = otherParams.distanceM - safeDistanceM
         -- too close, nothing better that we can do. Shouldn't happen ;)
  in  if | deltaDistanceM < 0   -> -comfortableDecelMss
         -- if the distance is 0.1, we treat it as 0 anyway, to avoid rounding errors
         -- when dividing by a very small number in the next case
         | deltaDistanceM < 0.1 -> 0
         | otherwise            -> computeAccel approachSpeedKph deltaDistanceM

minSeparationFromM obj = case obj of
  CarObj car -> car.sizeM.lengthM / 2 + 2
  TrafficLightObj tl -> 3
             
firstAheadOrDummyParams: Maybe (Obj, Float) -> ObjAheadParams
firstAheadOrDummyParams objAhead =
  case objAhead of
    Just (otherObj, distanceM) -> { speedKph = speedKphOfObj otherObj, 
                                    minSeparationM = minSeparationFromM otherObj,
                                    distanceM = distanceM }
    Nothing -> { speedKph = 0, 
                 minSeparationM = 0,
                 distanceM = 100000 } -- pretending there's an object far far away

accelForCar: [ Obj ] -> Car -> Float
accelForCar allObjs car =
  let firstAhead = NearestObstacle.findFirstAhead allObjs car
      firstAheadParams = firstAheadOrDummyParams firstAhead
  in  accelForCarGivenAhead car firstAheadParams

drive: Time -> [ Obj ] -> Car -> Car
drive t allObjs car =
  let accel = accelForCar allObjs car
      tS = t / 1000
      newSpeed = car.speedKph + accel * tS * 3600 / 1000 
      newSpeedNotNegative = max 0 newSpeed
      posM = car.posM
      distanceDeltaM = (speedKphToMps newSpeedNotNegative) * tS + accel * tS * tS / 2
      distanceDeltaMNotNegative = max 0 distanceDeltaM
      d = car.direction
      sind = sin d
      cosd = cos d
  in  { car | speedKph <- newSpeedNotNegative
            , posM <- { posM | xM <- posM.xM + distanceDeltaMNotNegative*cosd
                             , yM <- posM.yM + distanceDeltaMNotNegative*sind } } 