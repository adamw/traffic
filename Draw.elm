module Draw(drawObj, sortByDrawingOrder) where

import open Model
import open UIModel

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

drawTrafficLightCircle: Float -> TLState -> TLState -> Color -> Form
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

drawObj: WorldViewport -> Obj -> Maybe Form
drawObj worldViewport obj = 
  case obj of
    Car car -> Just (drawCar worldViewport car)
    TrafficLight trafficLight -> Just (drawTrafficLight worldViewport trafficLight)
    _ -> Nothing

sortByDrawingOrder: [ Obj ] -> [ Obj ]
sortByDrawingOrder objs =
  let (trafficLights, other) = partition isTrafficLight objs     
  in  -- drawing traffic lights last, so that they are on top
      other ++ trafficLights 