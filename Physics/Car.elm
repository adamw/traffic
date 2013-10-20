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

desiredDistanceM car objAheadParams =
  let decisionDistanceM = slowReactionTimeS * (speedKphToMps car.speedKph)
      extraSafetyDistanceM = 2
      minSeparationM = objAheadParams.minSeparationM + car.sizeM.lengthM / 2
  in  max (decisionDistanceM + extraSafetyDistanceM) minSeparationM

computeDecel dvKph ddM =
  let a = (speedKphToMps dvKph)^2/(2*ddM) 
  in  if | a < comfortableDecelMss -> 0
         | a > maxDecelMss -> -maxDecelMss
         | otherwise -> -a

zeroIfNegligible value bound = if (abs value) < bound then 0 else value

-- approach speed
deltaOfSpeedKph car objAheadParams = 
  let dvNegligibleKph = car.speedKph / 50 -- 0.1kph for 5kph difference, 1kph for 50kph
      dvKphRaw = car.speedKph - objAheadParams.speedKph
  in  zeroIfNegligible dvKphRaw dvNegligibleKph

deltaOfDistanceM dsM car objAheadParams =
  let ddNegligibleM = dsM/50 -- 0.1m for 5meters distance, 10m for 500meters
      ddMRaw = objAheadParams.distanceM - dsM
  in  zeroIfNegligible ddMRaw ddNegligibleM

speedAdjustDistanceM dvKph = (speedKphToMps dvKph)^2/(2*comfortableDecelMss)

{-- 
we are treating small distances and speeds as 0 to prevent frequent changes
in acceleration (accel/decel) and to prevent rounding errors when diving by a very
small number.
--}
accelForCarGivenAhead: Car -> ObjAheadParams -> Float
accelForCarGivenAhead car objAheadParams =
  let dvKph = deltaOfSpeedKph car objAheadParams
      dsM = desiredDistanceM car objAheadParams -- distance safe in meters       
      ddM = deltaOfDistanceM dsM car objAheadParams
      dsaM = speedAdjustDistanceM dvKph
  in  if -- exactly at target or very close (due to rounding)
         | ddM == 0 && dvKph == 0 -> 0          
         | ddM <= 0 -> -maxDecelMss
         | otherwise -> if dvKph < 0 
           -- drifting away, and above safe distance - can accelerate
           then car.aMss           
           else if ddM > dsaM 
                   -- we are further away then the speed adjust distance - can accel
                   then car.aMss
                   -- adjsuting speed
                   else computeDecel dvKph ddM

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
      newSpeed = car.speedKph + accel * tS * 18 / 5 -- 3600/1000
      newSpeedClamped = clamp 0 50 newSpeed -- we are safe
      posM = car.posM
      distanceDeltaM = (speedKphToMps newSpeedClamped) * tS + accel * tS * tS / 2
      distanceDeltaMNotNegative = max 0 distanceDeltaM
      d = car.direction
      sind = sin d
      cosd = cos d
  in  { car | speedKph <- newSpeedClamped
            , posM <- { posM | xM <- posM.xM + distanceDeltaMNotNegative*cosd
                             , yM <- posM.yM + distanceDeltaMNotNegative*sind } } 