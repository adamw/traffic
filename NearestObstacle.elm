module NearestObstacle(findFirstAhead) where

import open Model

type ObjAhead = Maybe (Obj, Float)

distanceIfAhead: Car -> Obj -> Maybe Float
distanceIfAhead fromCar toObj =
  let from = fromCar.posM
      to = posMOfObj toObj
      -- centring the target coordinates ('to') with 'from'
      toRelToFrom = { xM = to.xM - from.xM, yM = to.yM - from.yM }
      -- r is now the distance between the points, phi the angle
      (r, phi) = toPolar (toRelToFrom.xM, toRelToFrom.yM)
      -- if 'phi' is the same as the car direction, then it must be ahead
  in  if abs(phi - fromCar.direction) <= 0.01
      then Just r
      else Nothing

isTrafficLight obj = case obj of { TrafficLightObj tl -> True ; _ -> False }

updateIfNearer: Car -> Float -> Obj -> ObjAhead -> ObjAhead
updateIfNearer fromCar minTlDistM toObj current =
  let dist = distanceIfAhead fromCar toObj
      objIsTl = isTrafficLight toObj
      objIsTlAndTooClose = objIsTl && case dist of { Just d -> d < minTlDistM ; _ -> False }
  in  case (dist, current, objIsTlAndTooClose) of
        (Nothing, _, _)       -> current
        (_,       _, True)    -> current
        (Just d,  Nothing, _) -> Just (toObj, d)
        (Just d,  Just (otherObj, otherD), _) ->
          if (otherD > d) then Just (toObj, d) else current

{--
If the closest object is a traffic light, and it's closer than the given stopping
distance, then it's ignored (this is for the case of light changes, when the car
is too close to stop).
--}
findFirstAhead: [ Obj ] -> Car -> Float -> ObjAhead
findFirstAhead allObjs aheadOf stoppingDistanceM = 
  let isOtherObj = (\o -> o /= (CarObj aheadOf))
      -- green lights are 'transparent' - they are not treated as an obstacle
      isNotGreenLight = (\o -> case o of
                                 TrafficLightObj tl -> tl.state /= GreenTrafficLight
                                 _ -> True)
      isPotentialObstacle = (\o -> case o of
                                     CarObj c -> True
                                     TrafficLightObj tl -> True
                                     _ -> False)
      otherObjs = filter isNotGreenLight . 
        filter isOtherObj . 
        filter isPotentialObstacle <| allObjs
  in  foldl (updateIfNearer aheadOf stoppingDistanceM) Nothing otherObjs