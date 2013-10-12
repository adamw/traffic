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

updateIfNearer: Car -> Obj -> ObjAhead -> ObjAhead
updateIfNearer fromCar toObj current =
  let dist = distanceIfAhead fromCar toObj
  in  case (dist, current) of
        (Nothing, _)       -> current
        (Just d,  Nothing) -> Just (toObj, d)
        (Just d,  Just (otherObj, otherD)) ->
          if (otherD > d) then Just (toObj, d) else current

-- TODO: min non-green traffic light distance (closer than this - we don't stop
-- and we detect the next obstacle)
findFirstAhead: [ Obj ] -> Car -> ObjAhead
findFirstAhead allObjs aheadOf = 
  let isOtherObj = (\o -> o /= (CarObj aheadOf))
      -- green lights are 'transparent' - they are not treated as an obstacle
      isNotGreenLight = (\o -> case o of
                                 TrafficLightObj tl -> tl.state /= GreenTrafficLight
                                 _ -> True)
      otherObjs = filter isNotGreenLight . filter isOtherObj <| allObjs
  in  foldl (updateIfNearer aheadOf) Nothing otherObjs