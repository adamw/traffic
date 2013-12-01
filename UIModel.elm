module UIModel where

import open Model

-- c suffix means canvas
type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }

type WorldViewport = { viewportM: ViewportM, canvas: SizeC }

type UIWorld = { viewport: WorldViewport, 
                 world: World,
                 info: String,
                 tlInfo: String,
                 timeMultiplier: Float }              

-- WORLD UPDATES

updateViewportM: (ViewportM -> ViewportM) -> UIWorld -> UIWorld
updateViewportM updateFn uiworld =
  let oldWorldViewport = uiworld.viewport
      oldViewportM = oldWorldViewport.viewportM
      newViewportM = updateFn oldViewportM
      newWorldViewport = { oldWorldViewport | viewportM <- newViewportM }
  in  { uiworld | viewport <- newWorldViewport }

scaleViewport: Float -> UIWorld -> UIWorld
scaleViewport factor =                 
  updateViewportM (\oldViewportM ->
      let oldViewportSize = oldViewportM.sizeM
          newViewportSize = { lengthM = oldViewportSize.lengthM * factor, 
                              widthM  = oldViewportSize.widthM * factor }
      in  { oldViewportM | sizeM <- newViewportSize }
    )

panViewport: Float -> Float -> UIWorld -> UIWorld
panViewport xFactor yFactor = 
  updateViewportM (\oldViewportM ->
      let oldViewportCenter = oldViewportM.centerM
          newViewportCenter = { xM = oldViewportCenter.xM + xFactor * oldViewportM.sizeM.lengthM, 
                                yM = oldViewportCenter.yM + yFactor * oldViewportM.sizeM.widthM }
      in  { oldViewportM | centerM <- newViewportCenter }
    )

appendToInfo: String -> UIWorld -> UIWorld
appendToInfo what uiworld = { uiworld | info <- uiworld.info ++ what }

setTlInfo: String -> UIWorld -> UIWorld
setTlInfo what uiworld = { uiworld | tlInfo <- what }

tlChangedManuallyInfo: String
tlChangedManuallyInfo = "Traffic ligths are changed manually."

tlChangedAutomaticallyInfo: Int -> Int -> String
tlChangedAutomaticallyInfo lr td = concat [
    "Traffic lights are changed automatically: left-right every ",
    show lr, 
    " seconds, top-down every ",
    show td,
    " seconds"
  ]