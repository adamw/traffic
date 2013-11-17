module Physics.CarCreator(createIfVacant) where

import open Model

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

createIfVacant: CarCreator -> [ ObjWithDist ] -> Maybe Obj
createIfVacant cc objsAheadWithDist = 
  if (nextObjWithinCCPos cc objsAheadWithDist) then Nothing else Just (Car (createCar cc))