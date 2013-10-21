module UI(layout, worldStep, inputSignal) where

import open Model
import open WorldModel

import Graphics.Input
import Physics
import Draw

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput 
  | PanLeftInput | PanRightInput | PanUpInput | PanDownInput
  | ToggleTrafficLight
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
    ToggleTrafficLight -> updateWorldObjs world <| Physics.toggleTrafficLight
    TickInput t        -> updateWorldObjs world <| Physics.updateObjs t world.annihilator

-- LAYOUT

simulation world = 
  let { viewport, objs } = world
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      os = justs . map (Draw.drawObj viewport) <| objs
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ os)

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

debugObj world obj =
  case obj of
    CarObj car -> asText <| car -- accelForCar world.objs car -- findFirstAhead world.objs car 
    TrafficLightObj trafficLight -> asText <| trafficLight
    _ -> asText <| obj

debug world = 
  let debugs = map (debugObj world) world.objs
      x = "x"
  in  flow down (debugs ++ [ asText world.info ] ++ [ asText x ])

(tlToggleEl, tlToggleInput) = 
  let (btnEl, btnSignal) = Graphics.Input.button "Change traffic light" 
      btnInput = sampleOn btnSignal (constant ToggleTrafficLight)
  in  (btnEl, btnInput)  

-- VIEWPORT CONTROLS

viewportCtrlBtnsSpecs = [
  ("+", ZoomInInput),
  ("-", ZoomOutInput),
  ("left", PanLeftInput),
  ("right", PanRightInput),
  ("up", PanUpInput),
  ("down", PanDownInput) ]

createViewportCtrlBtn: (String, Input) -> (Element, Signal Input)
createViewportCtrlBtn (label, input) = 
  let (btnEl, btnSignal) = Graphics.Input.button label 
      btnInput = sampleOn btnSignal (constant input)
  in  (btnEl, btnInput)

(viewportCtrlBtnsLayout, viewportCtrlBtnsSignals) = 
  let created = map createViewportCtrlBtn viewportCtrlBtnsSpecs
      (btns, signals) = unzip created
      layedOut = flow right btns
  in  (layedOut, signals)

-- WIRE

ticker = TickInput <~ fps 25

inputSignal = merges (ticker :: tlToggleInput :: viewportCtrlBtnsSignals)

layout world = flow down [ simulation world, 
                           areaInfo world, 
                           --debug world, 
                           viewportCtrlBtnsLayout,
                           tlToggleEl ]