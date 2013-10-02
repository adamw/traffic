module Traffic where

import Mouse
import open Graphics.Input

-- MODEL

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }
type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float }

-- c suffix means canvas
type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }

type WorldViewport = { viewportM: ViewportM, canvas: SizeC }

-- UPDATE

drive: Time -> Car -> Car
drive t car =
  let posM = car.posM
      distanceDeltaM = (car.speedKph * 1000 / 3600) * (t / 1000)
      d = car.direction
      sind = sin d
      cosd = cos d
  in  { car | posM <- { posM | xM <- posM.xM + distanceDeltaM*cosd
                             , yM <- posM.yM + distanceDeltaM*sind } } 

-- TRANSLATION

trans: Float -> Float -> Float -> Float
trans dimM dimC v = (v/dimM)*dimC

posMToPosC: WorldViewport -> PosM -> PosC
posMToPosC { viewportM, canvas } posM = { 
  xC = trans viewportM.sizeM.lengthM canvas.widthC posM.xM,
  yC = trans viewportM.sizeM.widthM canvas.heightC posM.yM }

sizeMToSizeC: WorldViewport -> SizeM -> SizeC
sizeMToSizeC { viewportM, canvas } sizeM = {
  widthC = trans viewportM.sizeM.lengthM canvas.widthC sizeM.lengthM,
  heightC = trans viewportM.sizeM.widthM canvas.heightC sizeM.widthM }

drawCar: WorldViewport -> Car -> Form 
drawCar worldViewport car = 
  let sz  = sizeMToSizeC worldViewport car.sizeM
      pos = posMToPosC worldViewport car.posM
  in  rect sz.widthC sz.heightC |> outlined (solid red) 
                                |> move (pos.xC, pos.yC)
                                |> rotate car.direction

-- WORLD

mainCanvas: SizeC
mainCanvas = { widthC = 500, heightC = 250 }

initialViewportM: ViewportM
initialViewportM = { sizeM = { lengthM = 100, widthM = 50 },
                     centerM = { xM = 0, yM = 0 } }

initialCar: ViewportM -> Car
initialCar viewportM = { posM = { xM = -(viewportM.sizeM.lengthM / 2) + 10, yM = 0 }, 
                         speedKph = 10,
                         sizeM = { lengthM = 2, widthM = 1 },
                         direction = degrees 30  }

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

type World = { viewport: WorldViewport, cars: [ Car ], info: String }

initialWorld: World
initialWorld = { viewport = initialWorldViewport, 
                 cars = [ initialCar initialViewportM ], 
                 info = "X" }

scaleWorldArea: Float -> World -> World
scaleWorldArea factor world =                 
  let oldWorldViewport = world.viewport
      oldViewportM = oldWorldViewport.viewportM
      oldViewportSize = oldViewportM.sizeM
      newViewportSize = { lengthM = oldViewportSize.lengthM * factor, 
                          widthM = oldViewportSize.widthM * factor }
      newViewportM = { oldViewportM | sizeM <- newViewportSize }
      newWorldViewport = { oldWorldViewport | viewportM <- newViewportM }
  in  { world | viewport <- newWorldViewport }

appendToWorldInfo: String -> World -> World
appendToWorldInfo what world = { world | info <- world.info ++ what }

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput | TickInput Time

worldStep input world = 
  case input of
    ZoomInInput ->
      scaleWorldArea 0.5 . appendToWorldInfo "+" <| world
    ZoomOutInput ->
      scaleWorldArea 2 . appendToWorldInfo "-" <| world 
    TickInput t ->
      let cars = world.cars
          newCars = map (drive t) cars
      in  { world | cars <- newCars } 

-- LAYOUT

simulation world = 
  let { viewport, cars } = world
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      cs = map (drawCar viewport) cars
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ cs)

areaInfo world =
  let viewportM = world.viewport.viewportM
      parts = [ "Current area: length = ",
                show viewportM.sizeM.lengthM,
                ", width: ",
                show viewportM.sizeM.widthM,
                ", center at: (",
                show viewportM.centerM.xM,
                ", ",
                show viewportM.centerM.yM,
                ")" ]
      partsCombined = foldr (++) "" parts
  in  (asText partsCombined)

debug world = 
  let debugCar = asText <| drawCar world.viewport <| head world.cars 
  in  debugCar `above` (asText world.info)

-- WIRE

(buttons, buttonSignals) =
  let (plusBtnEl, plusBtnSignal) = button "+"
      (minusBtnEl, minusBtnSignal) = button "-"
      buttonsLayout = plusBtnEl `beside` minusBtnEl
      plusBtnInput = sampleOn plusBtnSignal (constant ZoomInInput)
      minusBtnInput = sampleOn minusBtnSignal (constant ZoomOutInput)
  in  (buttonsLayout, [ plusBtnInput, minusBtnInput ])

ticker = TickInput <~ fps 25

inputSignal = merges (ticker :: buttonSignals)

layout world = flow down [ simulation world, areaInfo world, debug world, buttons ]

main = layout <~ foldp worldStep initialWorld inputSignal
