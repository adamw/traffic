module Physics.Annihilator(annihilateIfOutOfBounds) where

import open Model

annihilateIfOutOfBounds: Annihilator -> AnyObj -> Maybe AnyObj
annihilateIfOutOfBounds ann obj =
  let objPosM = obj.posM
      shouldAnnihilate = objPosM.xM < ann.minX || 
        objPosM.xM > ann.maxX ||
        objPosM.yM < ann.minY ||
        objPosM.yM > ann.maxY
  in  if shouldAnnihilate then Nothing else Just obj