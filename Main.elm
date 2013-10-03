module Traffic where

import Mouse
import open Graphics.Input

-- MODEL

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }

type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float }

data TrafficLightState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight
type TrafficLight = { posM: PosM, state: TrafficLightState, direction: Float }

trafficLightSizeM: SizeM
trafficLightSizeM = { lengthM = 4, widthM = 8 }

trafficLightRadiusM = 1
trafficLightSeparationM = 0.25

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
  xC = trans viewportM.sizeM.lengthM canvas.widthC (posM.xM - viewportM.centerM.xM),
  yC = trans viewportM.sizeM.widthM canvas.heightC (posM.yM - viewportM.centerM.yM) }

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

orange = rgb 255 127 0

drawTrafficLightOval: Float -> Float -> TrafficLightState -> TrafficLightState -> Color -> Form
drawTrafficLightOval w h lightState fillIfState clr =
  let base = oval w h
  in  (if (lightState == fillIfState) 
       then (filled clr base) 
       else (outlined (solid clr) base))

drawTrafficLight: WorldViewport -> TrafficLight -> Form
drawTrafficLight ({ viewportM, canvas } as worldViewport) trafficLight =
  let sz  = sizeMToSizeC worldViewport trafficLightSizeM
      pos = posMToPosC worldViewport trafficLight.posM
      lightWidthC = trans viewportM.sizeM.lengthM canvas.widthC (trafficLightRadiusM*2)
      lightHeightC = trans viewportM.sizeM.widthM canvas.heightC (trafficLightRadiusM*2) 
      separationHeightC = trans viewportM.sizeM.widthM canvas.heightC trafficLightSeparationM
      lightOffsetC = lightHeightC+separationHeightC  
      boundingRect = rect sz.widthC sz.heightC |> outlined (solid black)
      baseOval = drawTrafficLightOval lightWidthC lightHeightC trafficLight.state
      redLight = baseOval RedTrafficLight red |> moveY lightOffsetC
      yellowLight = baseOval YellowTrafficLight orange 
      greenLight = baseOval GreenTrafficLight green |> moveY -lightOffsetC
      wholeLight = group [ boundingRect, redLight, yellowLight, greenLight ]
  in  wholeLight |> move (pos.xC, pos.yC)
                 |> rotate trafficLight.direction

-- WORLD

mainCanvas: SizeC
mainCanvas = { widthC = 500, heightC = 250 }

initialViewportM: ViewportM
initialViewportM = { sizeM = { lengthM = 200, widthM = 100 },
                     centerM = { xM = 0, yM = 0 } }

initialCar: ViewportM -> Car
initialCar viewportM = { posM = { xM = -(viewportM.sizeM.lengthM / 2) + 10, yM = 0 }, 
                         speedKph = 10,
                         sizeM = { lengthM = 4, widthM = 2 },
                         direction = degrees 30  }

initialTrafficLight: ViewportM -> TrafficLight
initialTrafficLight viewportM = { posM = { xM = 0, yM = 0 },
                                  state = RedTrafficLight,
                                  direction = degrees 0 }                         

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

type World = { viewport: WorldViewport, 
               cars: [ Car ], 
               trafficLights: [ TrafficLight ], 
               info: String }

initialWorld: World
initialWorld = { viewport = initialWorldViewport, 
                 cars = [ initialCar initialViewportM ], 
                 trafficLights = [ initialTrafficLight initialViewportM ], 
                 info = "X" }

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

-- WORLD SETUP

data Input = ZoomInInput | ZoomOutInput 
  | PanLeftInput | PanRightInput | PanUpInput | PanDownInput
  | TickInput Time

worldStep input world = 
  case input of
    ZoomInInput   -> scaleWorldViewport 0.5 . appendToWorldInfo "+" <| world
    ZoomOutInput  -> scaleWorldViewport 2 . appendToWorldInfo "-" <| world 
    PanLeftInput  -> panWorldViewport -0.5 0 . appendToWorldInfo "L" <| world
    PanRightInput -> panWorldViewport  0.5 0 . appendToWorldInfo "R" <| world
    PanUpInput    -> panWorldViewport 0  0.5 . appendToWorldInfo "U" <| world
    PanDownInput  -> panWorldViewport 0 -0.5 . appendToWorldInfo "D" <| world
    TickInput t   ->
      let cars = world.cars
          newCars = map (drive t) cars
      in  { world | cars <- newCars } 

-- LAYOUT

simulation world = 
  let { viewport, cars, trafficLights } = world
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      cs = map (drawCar viewport) cars
      ss = map (drawTrafficLight viewport) trafficLights
      boundary = rect canvas.widthC canvas.heightC |> outlined (solid black)
  in  collage w h ([ boundary ] ++ cs ++ ss)

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
  let (btnEl, btnSignal) = button label 
      btnInput = sampleOn btnSignal (constant input)
  in  (btnEl, btnInput)

(viewportCtrlBtnsLayout, viewportCtrlBtnsSignals) = 
  let created = map createViewportCtrlBtn viewportCtrlBtnsSpecs
      (btns, signals) = unzip created
      layedOut = flow right btns
  in  (layedOut, signals)

-- WIRE

ticker = TickInput <~ fps 25

inputSignal = merges (ticker :: viewportCtrlBtnsSignals)

layout world = flow down [ simulation world, 
                           areaInfo world, 
                           debug world, 
                           viewportCtrlBtnsLayout ]

main = layout <~ foldp worldStep initialWorld inputSignal
