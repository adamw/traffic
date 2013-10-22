module Physics.Annihilator(annihilateIfOutOfBounds) where

import open Model

annihilateIfOutOfBounds: Annihilator -> Obj -> Maybe Obj
annihilateIfOutOfBounds ann obj =
  let objPosM = posMOfObj obj
      shouldAnnihilate = objPosM.xM < ann.minX || 
        objPosM.xM > ann.maxX ||
        objPosM.yM < ann.minY ||
        objPosM.yM > ann.maxY
  in  if shouldAnnihilate then Nothing else Just obj