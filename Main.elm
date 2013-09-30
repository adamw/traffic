module Traffic where

import Mouse

-- MODEL
-- m suffix means meters

type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float }

initialCar: SizeM -> Car
initialCar area = { posM = { xM = -(mainAreaM.lengthM / 2) + 10, yM = 0 }, 
                    speedKph = 10,
                    sizeM = { lengthM = 2, widthM = 1 },
                    direction = degrees 0 }

-- UPDATE

drive: Float -> Car -> Car
drive t car =
  let posM = car.posM
      distanceDeltaM = (car.speedKph * 1000 / 3600) * (t / 1000)
  in  { car | posM <- { posM | xM <- posM.xM + distanceDeltaM } } 

type Sq = { x: Float, y: Float }

-- TRANSLATION
-- c suffix means canvas

type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }
type WorldSize = { area: SizeM, canvas: SizeC }

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
  in  rect sz.widthC sz.heightC |> outlined (solid red) |> move (pos.xC, pos.yC)

-- WORLD

mainCanvas: SizeC
mainCanvas = { widthC = 500, heightC = 250 }

mainAreaM: SizeM
mainAreaM = { lengthM = 100, widthM = 50 }

mainWorldSize = { area = mainAreaM, canvas = mainCanvas }

type World = { worldSize: WorldSize, cars: [ Car ] }

-- LAYOUT

simulation world = 
  let { worldSize, cars } = world
      { area, canvas } = worldSize
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      cs = map (drawCar worldSize) cars
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ cs)

debug world = asText <| drawCar world.worldSize <| head world.cars

layout world = (simulation world) `above` (debug world)

-- WORLD SETUP

worldStep t world = 
  let cars = world.cars
      newCars = map (drive t) cars
  in  { world | cars <- newCars } 

initialWorld = { worldSize = mainWorldSize, cars = [ initialCar mainAreaM ] }

-- WIRE

ticker = fps 25

main = lift layout (foldp worldStep initialWorld ticker)
