module Traffic where

import Mouse
import open Graphics.Input

-- CONSTANTS
oneSecond = 1000
halfSecond = oneSecond/2

-- MODEL

-- m suffix means meters
type PosM = { xM: Float, yM: Float }
type SizeM = { lengthM: Float, widthM: Float }
type ViewportM = { sizeM: sizeM, centerM: PosM }

type Car = { posM: PosM, speedKph: Float, sizeM: SizeM, direction: Float, aMss: Float }

data TrafficLightState = RedTrafficLight | YellowTrafficLight | GreenTrafficLight
type TrafficLight = { posM: PosM, state: TrafficLightState, direction: Float }

data Obj = CarObj Car | TrafficLightObj TrafficLight

posMOfObj: Obj -> PosM
posMOfObj obj =
  case obj of
    CarObj car -> car.posM
    TrafficLightObj trafficLight -> trafficLight.posM

speedKphOfObj: Obj -> Float
speedKphOfObj obj =
  case obj of
    CarObj car -> car.speedKph
    _ -> 0

speedKphToMps: Float -> Float
speedKphToMps speedKph = speedKph * 1000 / 3600

-- c suffix means canvas
type PosC = { xC: Float, yC: Float }
type SizeC = { widthC: Float, heightC: Float }

type WorldViewport = { viewportM: ViewportM, canvas: SizeC }

-- NEAREST OBSTACLE DETECTION

type ObjAhead = Maybe (Obj, Float)

distanceIfAhead: Car -> Obj -> Maybe Float
distanceIfAhead fromCar toObj =
  let from = fromCar.posM
      to = posMOfObj toObj
      -- centring the target coordinates ('to') with 'from'
      toRelToFrom = { xM = to.xM - from.xM, yM = to.yM - from.yM }
      -- r is now the distance between the points, phi the angle
      (r, phi) = toPolar (toRelToFrom.xM, toRelToFrom.yM)
      -- if 'phi' is the same as the car direction, then it must be ahead
  in  if abs(phi - fromCar.direction) <= 0.01
      then Just r
      else Nothing

updateIfNearer: Car -> Obj -> ObjAhead -> ObjAhead
updateIfNearer fromCar toObj current =
  let dist = distanceIfAhead fromCar toObj
  in  case (dist, current) of
        (Nothing, _)       -> current
        (Just d,  Nothing) -> Just (toObj, d)
        (Just d,  Just (otherObj, otherD)) ->
          if (otherD > d) then Just (toObj, d) else current

-- TODO: min non-green traffic light distance (closer than this - we don't stop
-- and we detect the next obstacle)
findFirstAhead: [ Obj ] -> Car -> ObjAhead
findFirstAhead allObjs aheadOf = 
  let isOtherObj = (\o -> o /= (CarObj aheadOf))
      -- green lights are 'transparent' - they are not treated as an obstacle
      isNotGreenLight = (\o -> case o of
                                 TrafficLightObj tl -> tl.state /= GreenTrafficLight
                                 _ -> True)
      otherObjs = filter isNotGreenLight . filter isOtherObj <| allObjs
  in  foldl (updateIfNearer aheadOf) Nothing otherObjs

-- UPDATE

{-
1. detect what's ahead - find the closest object
2. decide if we need to start slowing down
   - barrier - always if close enough
   - another car - if close enough and moving slower
3. if so, start slowing down
4. if we can accelerate, accelerate
   - only if the next car is far away enough

deceleration - maximum? how far ahead we are going to start stopping

yellow light: should be on long enough for a car with a distance < comfortable breaking
distance to pass
-}

-- interesting ref: 
-- http://cs.zstu.edu.cn/udpaloolapdu/Files/20101122180945.pdf
-- http://www.direct.gov.uk/prod_consum_dg/groups/dg_digitalassets/@dg/@en/@motor/documents/digitalasset/dg_188029.pdf

-- see http://en.wikipedia.org/wiki/Braking_distance#Total_stopping_distance
slowReactionTimeS = 2.5
fastReactionTimeS = 1.5

gAccelMss = 9.81
comfortableDecelMss = gAccelMss * 0.35
maxDecelMss = gAccelMss * 0.7

minObjSeparationM = 10

{--
s = att/2
v = at -> t = v/a

s = a(v/a)(v/a)/2 = vv/2a -> a = vv/2s
--}

{--
What is the desireable distance between two objects ('back' object follows the 'front' one)
travelling at the given speeds, if we can break with the given deceleration?
--}
desiredDistanceM: Float -> Float -> Float -> Float
desiredDistanceM speedKphBack speedKphFront decelMss =
  let decisionDistanceM = slowReactionTimeS * (speedKphToMps speedKphBack)
      extraSafetyDistanceM = 0 -- TODO
      -- TODO real min separation is 2 + obj_len/2
  in  max (decisionDistanceM + extraSafetyDistanceM) minObjSeparationM

computeAccel: Float -> Float -> Float
computeAccel approachSpeedKph deltaDistanceM =
  let a = (speedKphToMps approachSpeedKph)^2/(2*deltaDistanceM) 
  in  if | a < comfortableDecelMss -> 0
         | a > maxDecelMss -> -maxDecelMss
         | otherwise -> -a

accelForCarGivenAhead: Car -> (Float, Float) -> Float
accelForCarGivenAhead car (otherSpeedKph, distanceM) =
  let approachSpeedKph = car.speedKph - otherSpeedKph
      safeDistanceM = desiredDistanceM car.speedKph otherSpeedKph comfortableDecelMss
      deltaDistanceM = distanceM - safeDistanceM
         -- too close, nothing better that we can do. Shouldn't happen ;)
  in  if | deltaDistanceM < 0   -> -comfortableDecelMss
         -- if the distance is 0.1, we treat it as 0 anyway, to avoid rounding errors
         -- when dividing by a very small number in the next case
         | deltaDistanceM < 0.1 -> 0
         | otherwise            -> computeAccel approachSpeedKph deltaDistanceM
             
firstAheadOrDummyParams: Maybe (Obj, Float) -> (Float, Float)
firstAheadOrDummyParams objAhead =
  case objAhead of
    Just (otherObj, distanceM) -> (speedKphOfObj otherObj, distanceM)
    Nothing -> (0, 100000) -- pretending there's an object far far away

accelForCar: [ Obj ] -> Car -> Float
accelForCar allObjs car =
  let firstAhead = findFirstAhead allObjs car
      firstAheadParams = firstAheadOrDummyParams firstAhead
  in  accelForCarGivenAhead car firstAheadParams

drive: Time -> [ Obj ] -> Car -> Car
drive t allObjs car =
  let accel = accelForCar allObjs car
      tS = t / 1000
      newSpeed = car.speedKph + accel * tS * 3600 / 1000 
      newSpeedNotNegative = max 0 newSpeed
      posM = car.posM
      distanceDeltaM = (speedKphToMps newSpeedNotNegative) * tS + accel * tS * tS / 2
      distanceDeltaMNotNegative = max 0 distanceDeltaM
      d = car.direction
      sind = sin d
      cosd = cos d
  in  { car | speedKph <- newSpeedNotNegative
            , posM <- { posM | xM <- posM.xM + distanceDeltaMNotNegative*cosd
                             , yM <- posM.yM + distanceDeltaMNotNegative*sind } } 

updateObj: Time -> [ Obj ] -> Obj -> Obj
updateObj t allObjs obj =
  case obj of
    CarObj car -> CarObj (drive t allObjs car)
    _ -> obj

updateObjsTimeQuant: Time -> [ Obj ] -> [ Obj ]
updateObjsTimeQuant t objs = map (updateObj t objs) objs

{--
We assume that decisions can be changed twice every second. Hence if the time span to cover
is longer, we chunk it into smaller pieces.
--}
updateObjs: Time -> [ Obj ] -> [ Obj ]
updateObjs t objs = 
  if (t > halfSecond) 
    then updateObjs (t-halfSecond) (updateObjsTimeQuant halfSecond objs) 
    else updateObjsTimeQuant t objs 

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

trafficLightSizeC: SizeC
trafficLightSizeC = { widthC = 12, heightC = 24 }

trafficLightRadiusC = 2.5
trafficLightSeparationC = 1.7

drawTrafficLightCircle: Float -> TrafficLightState -> TrafficLightState -> Color -> Form
drawTrafficLightCircle r lightState fillIfState clr =
  let base = circle r
  in  (if (lightState == fillIfState) 
       then (filled clr base) 
       else (outlined (solid clr) base))

drawTrafficLight: WorldViewport -> TrafficLight -> Form
drawTrafficLight ({ viewportM, canvas } as worldViewport) trafficLight =
  let sz  = trafficLightSizeC
      pos = posMToPosC worldViewport trafficLight.posM
      lightOffsetC = 2*trafficLightRadiusC+trafficLightSeparationC  
      backingRect = rect sz.widthC sz.heightC |> filled white
      boundingRect = rect sz.widthC sz.heightC |> outlined (solid black)
      baseOval = drawTrafficLightCircle trafficLightRadiusC trafficLight.state
      redLight = baseOval RedTrafficLight red |> moveY lightOffsetC
      yellowLight = baseOval YellowTrafficLight orange 
      greenLight = baseOval GreenTrafficLight green |> moveY -lightOffsetC
      wholeLight = group [ backingRect, boundingRect, redLight, yellowLight, greenLight ]
  in  wholeLight |> move (pos.xC, pos.yC)
                 |> rotate trafficLight.direction

drawObj: WorldViewport -> Obj -> Form
drawObj worldViewport obj = 
  case obj of
    CarObj car -> drawCar worldViewport car
    TrafficLightObj trafficLight -> drawTrafficLight worldViewport trafficLight

-- WORLD

mainCanvas: SizeC
mainCanvas = { widthC = 1000, heightC = 500 }

initialViewportM: ViewportM
initialViewportM = { sizeM = { lengthM = 300, widthM = 150 },
                     centerM = { xM = 0, yM = 0 } }

createCar: Float -> Car
createCar xOffset = { posM = { xM = xOffset, yM = 0 }, 
                       speedKph = 50,
                       sizeM = { lengthM = 4, widthM = 2 },
                       direction = degrees 0,
                       aMss = 4.3 }

initialTrafficLight: ViewportM -> TrafficLight
initialTrafficLight viewportM = { posM = { xM = 0, yM = 0 },
                                  state = RedTrafficLight,
                                  direction = degrees 0 }                         

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

type World = { viewport: WorldViewport, 
               objs: [ Obj ],
               info: String }

initialWorld: World
initialWorld = { viewport = initialWorldViewport, 
                 objs = [ CarObj (createCar -150), 
                          CarObj (createCar -100), 
                          CarObj (createCar -50), 
                          TrafficLightObj (initialTrafficLight initialViewportM) ], 
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
      let objs = world.objs
          newObjs = updateObjs t objs
      in  { world | objs <- newObjs } 

-- LAYOUT

simulation world = 
  let { viewport, objs } = world
      { canvas } = viewport
      (w, h) = (round canvas.widthC, round canvas.heightC) 
      os = map (drawObj viewport) objs
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

debug world = 
  let debugs = map (debugObj world) world.objs
      x = "x"
  in  flow down (debugs ++ [ asText world.info ] ++ [ asText x ])

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
