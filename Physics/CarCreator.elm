module Physics.CarCreator(createIfVacant) where

import Random.Generator
import open Model

smallCarBase     = { sizeM = { lengthM = 3, widthM = 1.8 },
                     aMss = 4.3 }

mediumCarBase    = { sizeM = { lengthM = 4, widthM = 2 },
                     aMss = 4.3 }

fastCarBase      = { sizeM = { lengthM = 4, widthM = 1.8 },
                     aMss = 6.0 }

smallTruckBase   = { sizeM = { lengthM = 6, widthM = 2 },
                     aMss = 3.5 }

bigTruckBase     = { sizeM = { lengthM = 8, widthM = 2.2 },
                     aMss = 3.0 }

randomBase: RandStdGen -> ({ sizeM: SizeM, aMss: Float }, RandStdGen)
randomBase rand =
  let (baseType, rand') = Random.Generator.int32Range (0, 9) rand              
      base = case baseType of
              0 -> smallCarBase
              1 -> smallCarBase
              2 -> mediumCarBase
              3 -> mediumCarBase
              4 -> mediumCarBase
              5 -> fastCarBase
              6 -> fastCarBase
              7 -> smallTruckBase
              8 -> smallTruckBase
              9 -> bigTruckBase
  in  (base, rand')

createCar: CarCreator -> RandStdGen -> (Car, RandStdGen)
createCar cc rand = 
  let (base0, rand') = randomBase rand
      base1 = { base0 | posM = cc.posM }
      base2 = { base1 | speedKph = 0 }      
      base3 = { base2 | direction = cc.direction }
      base4 = { base3 | clusterId = cc.clusterId }
  in  (base4, rand')

nextObjWithinCCPos cc objsAheadWithDist = 
  case objsAheadWithDist of
    [] -> False
    objWithDist :: _ -> objWithDist.distMToPrev < 5

createIfVacant: CarCreator -> [ ObjWithDist ] -> RandStdGen -> (Maybe Obj, RandStdGen)
createIfVacant cc objsAheadWithDist rand = 
  if (nextObjWithinCCPos cc objsAheadWithDist) 
    then (Nothing, rand) 
    else let (car, rand') = createCar cc rand in (Just (Car car), rand')