module UI(layout, uiworldStep, inputSignal) where

import open Model
import open UIModel

import Graphics.Input
import Physics
import Draw
import SimulationSpeed

import Physics.ObjOrderer

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput 
  | PanLeftInput | PanRightInput | PanUpInput | PanDownInput
  | ToggleTrafficLightInput
  | SpeedUpInput | SlowDownInput
  | TickInput Time

updateWorld uiworld updateFn =
  let world = uiworld.world
      newWorld = updateFn world
  in  { uiworld | world <- newWorld } 

uiworldStep: Input -> UIWorld -> UIWorld
uiworldStep input uiworld = 
  case input of
    ZoomInInput        -> scaleViewport 0.5 . appendToInfo "+" <| uiworld
    ZoomOutInput       -> scaleViewport 2 . appendToInfo "-" <| uiworld 
    PanLeftInput       -> panViewport -0.5 0 . appendToInfo "L" <| uiworld
    PanRightInput      -> panViewport  0.5 0 . appendToInfo "R" <| uiworld
    PanUpInput         -> panViewport 0  0.5 . appendToInfo "U" <| uiworld
    PanDownInput       -> panViewport 0 -0.5 . appendToInfo "D" <| uiworld
    SpeedUpInput       -> SimulationSpeed.speedOfSimulationUp uiworld
    SlowDownInput      -> SimulationSpeed.speedOfSimulationDown uiworld
    ToggleTrafficLightInput -> updateWorld uiworld <| Physics.toggleTrafficLight
    TickInput t        -> 
      let updateFn = Physics.updateObjs (SimulationSpeed.adjustTime uiworld t)
      in  updateWorld uiworld <| updateFn

-- LAYOUT

simulation uiworld = 
  let { viewport, world } = uiworld
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC)
      sorted = Draw.sortByDrawingOrder world.objs 
      os = justs . map (Draw.drawObj viewport) <| sorted
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ os)

uiworldInfo uiworld =
  let viewportM = uiworld.viewport.viewportM
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
      speedString = "Current simulation speed: " ++ (show uiworld.timeMultiplier) ++ "x"
      speedText = plainText speedString
  in  flow down [ areaPartsText, speedText ]

debugObj uiworld obj =
  case obj of
    CarObj car -> asText <| car -- accelForCar uiworld.objs car -- findFirstAhead uiworld.objs car 
    TrafficLightObj trafficLight -> asText <| trafficLight
    _ -> asText <| obj

debug uiworld = 
  let debugs = map (debugObj uiworld) uiworld.world.objs    
  in  flow down ({--debugs ++--} [ asText uiworld.info ])

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

inputSignal: Signal Input
inputSignal = merges (ticker :: tlToggleInput :: 
  speedUpInput :: slowDownInput ::
  viewportCtrlBtnsSignals)

layout: UIWorld -> Element
layout uiworld = flow down [ simulation uiworld, 
                             uiworldInfo uiworld, 
                             -- debug uiworld, 
                             viewportCtrlBtnsLayout,
                             tlToggleEl,
                             simSpeedCtrlsLayout ]