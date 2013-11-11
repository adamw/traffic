module Physics.CarCreator(createCars) where

import open Model
import Dict

createCar: CarCreator -> Car
createCar cc = { posM = cc.posM, 
                 speedKph = 0,
                 sizeM = { lengthM = 4, widthM = 2 },
                 direction = cc.direction,
                 aMss = 4.3,
                 clusterId = cc.clusterId }

nextObjWithinCCPos cc objsAheadWithDist = 
  case objsAheadWithDist of
    [] -> False
    objWithDist :: _ -> objWithDist.distMToPrev < 5

createIfVacant: CarCreator -> [ MaybeCarWithDist ] -> Maybe Car
createIfVacant cc objsAheadWithDist = 
  if (nextObjWithinCCPos cc objsAheadWithDist) then Nothing else Just (createCar cc)

createCarsFor objsByCluster cc world =
  let objsAheadWithDist = Dict.findWithDefault [] cc.clusterId objsByCluster
      maybeNewCar = createIfVacant cc objsAheadWithDist
  in  case maybeNewCar of
        Just car -> { world | cars <- car :: world.cars }
        Nothing -> world

createCars: Dict.Dict ClusterId [ MaybeCarWithDist ] -> World -> World
createCars objsByCluster world =
  foldl (createCarsFor objsByCluster) world world.carCreators