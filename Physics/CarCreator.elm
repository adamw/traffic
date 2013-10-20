module Physics.CarCreator(createIfVacant) where

import open Model

createCar: CarCreator -> Car
createCar cc = { posM = cc.posM, 
                 speedKph = 0,
                 sizeM = { lengthM = 4, widthM = 2 },
                 direction = cc.direction,
                 aMss = 4.3 }

objWithinCCPos cc obj =
  if (CarCreatorObj cc == obj) 
    then False 
    else (distM (posMOfObj obj) cc.posM) < 5

anyObjWithinCCPos cc allObjs = any (objWithinCCPos cc) allObjs

createIfVacant: CarCreator -> [ Obj ] -> Maybe Obj
createIfVacant cc allObjs = 
  if (anyObjWithinCCPos cc allObjs) then Nothing else Just (CarObj (createCar cc))