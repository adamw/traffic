module Physics(update, switchTrafficLights) where 

import open Model
import Physics.Car
import Physics.TrafficLights
import Physics.CarCreator
import Physics.Annihilator
import Physics.ObjOrderer
import Dict

updateObjCluster: Time -> [ Car ] -> [ MaybeCarWithDist ] -> [ Car ]
updateObjCluster t acc objsWithDist  =
  case objsWithDist of
    [] -> acc
    objWithDist :: tl ->
      case objWithDist.car of
        Just car -> 
          let updated = Physics.Car.drive t tl car
          in  updateObjCluster t (updated ++ acc) tl 
        Nothing -> updateObjCluster t acc tl

updateCars t world =
  let tlsNotGreen = filter (\tl -> tl.state != GreenTrafficLight) world.tls
      orderedObjsByCluster = Physics.ObjOrderer.orderedObjClusters world.cars tlsNotGreen
      updatedObjsByCluster = Dict.map (updateObjCluster t []) orderedObjsByCluster
      updated = concat updatedObjsByCluster
      updatedWorld = { world | cars <- updated }
  in  CarCreator.createCars updatedObjsByCluster updatedWorld

updateAnn world =
  let annFn = Physics.Annihilator.annihilateIfOutOfBounds world.ann
      inBoundsCars = justs <| map annFn world.cars
  in  { world | cars <- inBoundsCars }

updateTimeQuant: Time -> World -> World
updateTimeQuant t world = updateCars t . updateAnn . Physics.TrafficLights.update t

timeQuantMs = 100

{--
We assume that decisions can be changed every 100ms. Hence if the time span to cover
is longer, we chunk it into smaller pieces.
--}
update: Time -> World -> World
update t world = 
  if (t > timeQuantMs) 
    then update (t-timeQuantMs) (updateTimeQuant timeQuantMs world) 
    else updateTimeQuant t world

switchTrafficLights: World -> World
switchTrafficLights world = Physics.TrafficLights.startSwitch world 

