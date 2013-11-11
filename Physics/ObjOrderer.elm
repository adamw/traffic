module Physics.ObjOrderer(orderedObjClusters) where

import open Model
import Dict
import Util

type MaybeCar = { car: Maybe Car, obj: AnyObj }
type MaybeCarsByCluster = Dict.Dict ClusterId [ MaybeCar ]

orderedObjClusters: [ Car ] -> [ TrafficLight ] -> Dict.Dict ClusterId [ MaybeCarWithDist ]
orderedObjClusters cars tls = 
  let toCluster = (carsToMaybeCar cars) ++ (tlsToMaybeCar tls)
      clusters = clusterMcs toCluster Dict.empty
      sortedClusters = Dict.map (\c -> Util.mergeSort (clusterLtFn c) c) clusters
  in  Dict.map mcsToWithDist sortedClusters 

-- 

carsToMaybeCar: [ Car ] -> [ MaybeCar ]
carsToMaybeCar = map (\c -> { car = Just c, obj = c })

tlsToMaybeCar: [ TrafficLight ] -> [ MaybeCar ]
tlsToMaybeCar = map (\tl -> { car = Nothing, obj = tl })

clusterMcs: [ MaybeCar ] -> MaybeCarsByCluster -> MaybeCarsByCluster
clusterMcs mcs acc =
  case mcs of
    [] -> acc
    mc :: otherMcs -> 
      let clusterId = mc.obj.clusterId
          currentClusterMcs = maybe [] id (Dict.lookup clusterId acc)
      in  clusterMcs otherMcs (Dict.insert clusterId (mc :: currentClusterMcs) acc)

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
If two objects have the same position, we order them basing on type: cars come
last.
--}
samePosMcLtFn mc1 mc2 = if isJust mc1.car then False else True

clusterLtFn: [ MaybeCar ] -> (MaybeCar -> MaybeCar -> Bool)
clusterLtFn mcs =
  case mcs of
    [] -> (\mc1 -> \mc2 -> True)
    mc :: _ -> 
      let direction = mc.obj.direction 
          posMLt = posMLtFn direction
      in  \mc1 -> \mc2 -> 
        let pM1 = mc1.obj.posM
            pM2 = mc2.obj.posM
            pMEqual = (coordEq pM1.xM pM2.xM) && (coordEq pM1.yM pM2.yM)
        in  if (pMEqual) 
            then samePosMcLtFn mc1 mc2 
            else posMLt (pM1, pM2)

mcToWithDist (mc, prevMc) =
  { mc | distMToPrev = distM mc.obj.posM prevMc.obj.posM }

mcsToWithDist mcs = 
  case mcs of
    [] -> []
    mc :: tl -> 
      let mcsAndPrevPairs = zip tl mcs
          firstWithDist = { mc | distMToPrev = 0 }
      in  firstWithDist :: (map mcToWithDist mcsAndPrevPairs)
