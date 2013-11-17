module Physics.ObjOrderer(orderedObjClusters) where

import open Model
import Dict
import Util

orderedObjClusters: [ Obj ] -> [ [ ObjWithDist ] ]
orderedObjClusters objs = 
  let clusters = clusterObjs objs Dict.empty
      sortedClusters = map (\c -> Util.mergeSort (clusterLtFn c) c) clusters
  in  map objsToWithDist sortedClusters 

-- 

clusterObjs: [ Obj ] -> Dict.Dict Int [ Obj ] -> [ [ Obj ] ]
clusterObjs objs acc =
  case objs of
    [] -> Dict.values acc
    obj :: otherObjs -> 
      let clusterId = clusterIdOfObj obj
          currentClusterObjs = maybe [] id (Dict.lookup clusterId acc)
      in  clusterObjs otherObjs (Dict.insert clusterId (obj :: currentClusterObjs) acc)

degrees0   = degrees 0
degrees90  = degrees 90
degrees180 = degrees 180
degrees270 = degrees 270

createPosMLt ltX ltY = \(pM1, pM2) ->
  let xMEq = coordEq pM1.xM pM2.xM
  in  if xMEq then ltY pM1.yM pM2.yM else ltX pM1.xM pM2.xM

{--
sorting order (so that objects "behind" come first") depends on the direction:
  - degree <0, 90) -> x+, y+
  - degree <90, 180) -> x-, y+
  - degree <180, 270) -> x-, y-
  - degree <270, 0) -> x+, y-
--}
posMLtFn: Float -> ((PosM, PosM) -> Bool)
posMLtFn direction = 
  if | direction >= degrees0   && direction < degrees90  -> createPosMLt (<) (<)
     | direction >= degrees90  && direction < degrees180 -> createPosMLt (>) (<)
     | direction >= degrees180 && direction < degrees270 -> createPosMLt (>) (>)
     | otherwise                                         -> createPosMLt (<) (>)   

{--
If two objects have the same position, we order them basing on type.
Car creators come first, then traffic lights. The important thing is that
cars come last, so that if a car creator creates a car, the car is "after"
the creator, and so is visible when checking if next car can be created. 
Similarly with TL.
--}
samePosObjLtFn o1 o2 =
  case (o1, o2) of
    (CarCreatorObj cc, _) -> True
    (TrafficLightObj tl, _) -> True
    _ -> False

clusterLtFn: [ Obj ] -> (Obj -> Obj -> Bool)
clusterLtFn objs =
  case objs of
    [] -> (\o1 -> \o2 -> True)
    obj :: _ -> 
      let direction = directionOfObj obj 
          posMLt = posMLtFn direction
      in  \o1 -> \o2 -> 
        let pM1 = posMOfObj o1
            pM2 = posMOfObj o2
            pMEqual = (coordEq pM1.xM pM2.xM) && (coordEq pM1.yM pM2.yM)
        in  if (pMEqual) 
            then samePosObjLtFn o1 o2 
            else posMLt (posMOfObj o1, posMOfObj o2)

objToWithDist (obj, prevObj) =
  { obj = obj, distMToPrev = distM (posMOfObj obj) (posMOfObj prevObj) }

objsToWithDist objs = 
  case objs of
    [] -> []
    obj :: tl -> 
      let objAndPrevPairs = zip tl objs
          firstWithDist = { obj = obj, distMToPrev = 0 }
      in  firstWithDist :: (map objToWithDist objAndPrevPairs)
