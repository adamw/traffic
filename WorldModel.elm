module WorldModel where

import open Model

-- c suffix means canvas
type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }

type WorldViewport = { viewportM: ViewportM, canvas: SizeC }

type World = { viewport: WorldViewport, 
               objs: [ Obj ],
               annihilator: Annihilator,
               info: String }              

-- WORLD UPDATES

updateWorldViewportM: (ViewportM -> ViewportM) -> World -> World
updateWorldViewportM updateFn world =
  let oldWorldViewport = world.viewport
      oldViewportM = oldWorldViewport.viewportM
      newViewportM = updateFn oldViewportM
      newWorldViewport = { oldWorldViewport | viewportM <- newViewportM }
  in  { world | viewport <- newWorldViewport }

scaleWorldViewport: Float -> World -> World
scaleWorldViewport factor =                 
  updateWorldViewportM (\oldViewportM ->
      let oldViewportSize = oldViewportM.sizeM
          newViewportSize = { lengthM = oldViewportSize.lengthM * factor, 
                              widthM  = oldViewportSize.widthM * factor }
      in  { oldViewportM | sizeM <- newViewportSize }
    )

panWorldViewport: Float -> Float -> World -> World
panWorldViewport xFactor yFactor = 
  updateWorldViewportM (\oldViewportM ->
      let oldViewportCenter = oldViewportM.centerM
          newViewportCenter = { xM = oldViewportCenter.xM + xFactor * oldViewportM.sizeM.lengthM, 
                                yM = oldViewportCenter.yM + yFactor * oldViewportM.sizeM.widthM }
      in  { oldViewportM | centerM <- newViewportCenter }
    )

appendToWorldInfo: String -> World -> World
appendToWorldInfo what world = { world | info <- world.info ++ what }