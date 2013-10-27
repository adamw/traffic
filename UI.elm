module UI(layout, worldStep, inputSignal) where

import open Model
import open WorldModel

import Graphics.Input
import Physics
import Draw
import SimulationSpeed

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput 
  | PanLeftInput | PanRightInput | PanUpInput | PanDownInput
  | ToggleTrafficLightInput
  | SpeedUpInput | SlowDownInput
  | TickInput Time

updateWorldObjs world updateFn =
  let objs = world.objs
      newObjs = updateFn objs
  in  { world | objs <- newObjs } 

worldStep input world = 
  case input of
    ZoomInInput        -> scaleWorldViewport 0.5 . appendToWorldInfo "+" <| world
    ZoomOutInput       -> scaleWorldViewport 2 . appendToWorldInfo "-" <| world 
    PanLeftInput       -> panWorldViewport -0.5 0 . appendToWorldInfo "L" <| world
    PanRightInput      -> panWorldViewport  0.5 0 . appendToWorldInfo "R" <| world
    PanUpInput         -> panWorldViewport 0  0.5 . appendToWorldInfo "U" <| world
    PanDownInput       -> panWorldViewport 0 -0.5 . appendToWorldInfo "D" <| world
    SpeedUpInput       -> SimulationSpeed.speedOfSimulationUp world
    SlowDownInput      -> SimulationSpeed.speedOfSimulationDown world
    ToggleTrafficLightInput -> updateWorldObjs world <| Physics.toggleTrafficLight
    TickInput t        -> 
      let updateFn = Physics.updateObjs (SimulationSpeed.adjustTime world t) world.annihilator
      in  updateWorldObjs world <| updateFn

-- LAYOUT

simulation world = 
  let { viewport, objs } = world
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC)
      sorted = Draw.sortByDrawingOrder objs 
      os = justs . map (Draw.drawObj viewport) <| sorted
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ os)

worldInfo world =
  let viewportM = world.viewport.viewportM
      areaParts = [ "Current area: length = ",
                show viewportM.sizeM.lengthM,
                ", width: ",
                show viewportM.sizeM.widthM,
                ", center at: (",
                show viewportM.centerM.xM,
                ", ",
                show viewportM.centerM.yM,
                ")" ]
      areaPartsCombined = foldr (++) "" areaParts
      areaPartsText = plainText areaPartsCombined
      speedString = "Current simulation speed: " ++ (show world.timeMultiplier) ++ "x"
      speedText = plainText speedString
  in  flow down [ areaPartsText, speedText ]

debugObj world obj =
  case obj of
    CarObj car -> asText <| car -- accelForCar world.objs car -- findFirstAhead world.objs car 
    TrafficLightObj trafficLight -> asText <| trafficLight
    _ -> asText <| obj

debug world = 
  let debugs = map (debugObj world) world.objs      
  in  flow down ({--debugs ++--} [ asText world.info ])

buttonEmittingInput text input =
  let (btnEl, btnSignal) = Graphics.Input.button text 
      btnInput = sampleOn btnSignal (constant input)
  in  (btnEl, btnInput) 

(tlToggleEl, tlToggleInput) = buttonEmittingInput "Change traffic light" ToggleTrafficLightInput

-- VIEWPORT CONTROLS

viewportCtrlBtnsSpecs = [
  ("+", ZoomInInput),
  ("-", ZoomOutInput),
  ("left", PanLeftInput),
  ("right", PanRightInput),
  ("up", PanUpInput),
  ("down", PanDownInput) ]

(viewportCtrlBtnsLayout, viewportCtrlBtnsSignals) = 
  let created = map (uncurry buttonEmittingInput) viewportCtrlBtnsSpecs
      (btns, signals) = unzip created
      layedOut = flow right btns
  in  (layedOut, signals)

-- TIME CONTROLS

(speedUpEl, speedUpInput) = buttonEmittingInput "Speed up" SpeedUpInput
(slowDownEl, slowDownInput) = buttonEmittingInput "Slow down" SlowDownInput

simSpeedCtrlsLayout = flow right [ speedUpEl, slowDownEl ] 

-- WIRE

ticker = TickInput <~ fps 25

inputSignal = merges (ticker :: tlToggleInput :: 
  speedUpInput :: slowDownInput ::
  viewportCtrlBtnsSignals)

layout world = flow down [ simulation world, 
                           worldInfo world, 
                           -- debug world, 
                           viewportCtrlBtnsLayout,
                           tlToggleEl,
                           simSpeedCtrlsLayout ]