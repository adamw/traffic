module UI(uiworldStep, inputSignal, layoutSignal) where

import open Model
import open UIModel
import open Util

import Graphics.Input
import Physics
import Draw
import SimulationSpeed
import Happiness
import String

import Physics.ObjOrderer

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput 
  | PanLeftInput | PanRightInput | PanUpInput | PanDownInput
  | ManualToggleTrafficLightsInput | AutoToggleTrafficLightsInput Int Int 
  | SpeedUpInput | SlowDownInput
  | TickInput Time

updateWorld updateFn uiworld  =
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
    ManualToggleTrafficLightsInput -> 
      setTlInfo tlChangedManuallyInfo . updateWorld Physics.switchTrafficLights <| uiworld
    AutoToggleTrafficLightsInput lr td -> 
      setTlInfo (tlChangedAutomaticallyInfo lr td) . id <| uiworld
    TickInput t        -> 
      let adjustedT = (SimulationSpeed.adjustTime uiworld t)
          updateFnP = Physics.update adjustedT
          updateFnH = Happiness.update adjustedT
      in  updateWorld (updateFnP . updateFnH) uiworld 

-- LAYOUT

background = toForm <| image 1100 550 "background.png"

elHeight = 40
col1ElWidth = 130+50+130+50

simulation uiworld = 
  let { viewport, world } = uiworld
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC)
      sorted = Draw.sortByDrawingOrder world.objs 
      os = justs . map (Draw.drawObj viewport) <| sorted
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ background, boundary ] ++ os)

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
    Car car -> asText <| car -- accelForCar uiworld.objs car -- findFirstAhead uiworld.objs car 
    TrafficLight trafficLight -> asText <| trafficLight
    _ -> asText <| obj

debug uiworld = 
  let debugs = map (debugObj uiworld) uiworld.world.objs    
  in  flow down ({--debugs ++--} [ asText uiworld.info ])

buttonEmittingInput text input =
  let (btnEl, btnSignal) = Graphics.Input.button text 
      btnInput = sampleOn btnSignal (constant input)
  in  (btnEl, btnInput) 

-- TRAFFIC LIGHTS

(tlToggleElSig, tlAutoToggleInput, tlManualToggleInput) = 
  let -- interface elements
      (lrIntFieldSig, lrIntValStr) = Graphics.Input.field "1"
      (tdIntFieldSig, tdIntValStr) = Graphics.Input.field "1"
      -- the signal value will be ignored, just the presence of the signal is important
      (autoToggleBtn, autoToggleRawInput) = 
        buttonEmittingInput "Change traffic lights automatically"  True   
      -- manual
      (manualToggleBtn, manualToggleInput) = buttonEmittingInput 
        "Change traffic lights manually" ManualToggleTrafficLightsInput   
      -- we don't want field changes to trigger the main signal; we only need to get
      -- the current value
      toIntOr1 = (getOrElse 1) . String.toInt
      lrIntVal = sampleOn autoToggleRawInput <| toIntOr1 <~ lrIntValStr
      tdIntVal = sampleOn autoToggleRawInput <| toIntOr1 <~ tdIntValStr
      autoToggleInput =  
        (\t -> \lr -> \td -> AutoToggleTrafficLightsInput lr td) <~
        autoToggleRawInput ~ lrIntVal ~ tdIntVal
      -- layout
      autoToggleEl lrIntEl tdIntEl = flow right [ 
        container 130 elHeight midLeft <| plainText "Left-right interval: ",
        size 50 elHeight <| lrIntEl,
        container 130 elHeight midLeft <| plainText "Top-down interval: ",
        size 50 elHeight <| tdIntEl,
        size 150 elHeight autoToggleBtn,
        size 150 elHeight manualToggleBtn ]
      autoToggleElSig = autoToggleEl <~ lrIntFieldSig ~ tdIntFieldSig
  in  (autoToggleElSig, autoToggleInput, manualToggleInput)

-- HAPPINESS

happinessInfo: UIWorld -> Element
happinessInfo uiworld = 
  let str = "Average happiness: " ++ (show <| Happiness.average uiworld.world) ++ "."
  in  container col1ElWidth elHeight midLeft <| plainText str

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

simSpeedLayout uiworld = 
  let speedString = " Current simulation speed: " ++ (show uiworld.timeMultiplier) ++ "x"
  in  flow right [
        container col1ElWidth elHeight midLeft <| plainText speedString, 
        size 150 elHeight speedUpEl, 
        size 150 elHeight slowDownEl
      ] 

-- WIRE

ticker = TickInput <~ fps 25

inputSignal: Signal Input
inputSignal = merges (ticker :: 
  tlManualToggleInput :: tlAutoToggleInput ::
  speedUpInput :: slowDownInput ::
  viewportCtrlBtnsSignals)

layoutSignal: Signal (UIWorld -> Element)
layoutSignal = 
  let layoutFn tlToggleEl uiworld = 
    flow down [ simulation uiworld, 
                -- uiworldInfo uiworld, 
                -- debug uiworld, 
                -- viewportCtrlBtnsLayout,
                happinessInfo uiworld,
                container col1ElWidth elHeight midLeft <| plainText uiworld.tlInfo, 
                tlToggleEl,
                simSpeedLayout uiworld ]
  in  layoutFn <~ tlToggleElSig