module Traffic where

import Mouse
import open Graphics.Input

-- MODEL

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float }

-- c suffix means canvas
type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }

type WorldSize = { area: SizeM, canvas: SizeC }

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

posMToPosC: WorldSize -> PosM -> PosC
posMToPosC { area, canvas } posM = { 
  xC = trans area.lengthM canvas.widthC posM.xM,
  yC = trans area.widthM canvas.heightC posM.yM }

sizeMToSizeC: WorldSize -> SizeM -> SizeC
sizeMToSizeC { area, canvas } sizeM = {
  widthC = trans area.lengthM canvas.widthC sizeM.lengthM,
  heightC = trans area.widthM canvas.heightC sizeM.widthM }

drawCar: WorldSize -> Car -> Form 
drawCar world car = 
  let sz  = sizeMToSizeC world car.sizeM
      pos = posMToPosC world car.posM
  in  rect sz.widthC sz.heightC |> outlined (solid red) 
                                |> move (pos.xC, pos.yC)
                                |> rotate car.direction

-- WORLD

mainCanvas: SizeC
mainCanvas = { widthC = 500, heightC = 250 }

initialAreaM: SizeM
initialAreaM = { lengthM = 100, widthM = 50 }

initialCar: SizeM -> Car
initialCar area = { posM = { xM = -(area.lengthM / 2) + 10, yM = 0 }, 
                    speedKph = 10,
                    sizeM = { lengthM = 2, widthM = 1 },
                    direction = degrees 30  }

initialWorldSize: WorldSize
initialWorldSize = { area = initialAreaM, canvas = mainCanvas }

type World = { worldSize: WorldSize, cars: [ Car ], info: String }

initialWorld: World
initialWorld = { worldSize = initialWorldSize, 
                 cars = [ initialCar initialAreaM ], 
                 info = "X" }

scaleWorldArea: Float -> World -> World
scaleWorldArea factor world =                 
  let oldWorldSize = world.worldSize
      oldArea = oldWorldSize.area
      newArea = { lengthM = oldArea.lengthM * factor, 
                  widthM = oldArea.widthM * factor }
      newWorldSize = { oldWorldSize | area <- newArea }
  in  { world | worldSize <- newWorldSize }

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
  let { worldSize, cars } = world
      { area, canvas } = worldSize
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      cs = map (drawCar worldSize) cars
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ cs)

areaInfo world =
  let area = world.worldSize.area
      parts = [ "Current area: length = ",
                show area.lengthM,
                ", width: ",
                show area.widthM ]
      partsCombined = foldr (++) "" parts
  in  (asText partsCombined)

debug world = 
  let debugCar = asText <| drawCar world.worldSize <| head world.cars 
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
