module Physics(update, manualTrafficLightsSwitch, startAutoTrafficLightsSwitch) where 

import open Model
import Physics.Car
import Physics.TrafficLights
import Physics.CarCreator
import Physics.Annihilator
import Physics.ObjOrderer

updateObj: Time -> [ ObjWithDist ] -> RandStdGen -> Obj -> ([ Obj ], RandStdGen)
updateObj t objsAheadWithDist rand obj =
  case obj of
    Car car -> 
      let updated = Car (Physics.Car.drive t objsAheadWithDist car)
      in  ([ updated ], rand)
    CarCreator cc -> 
      let (maybeNew, rand') = Physics.CarCreator.createIfVacant cc objsAheadWithDist rand
      in  (cons maybeNew [ obj ], rand')
    _ -> ([ obj ], rand)

updateObjCluster: Time -> [ Obj ] -> RandStdGen -> [ ObjWithDist ] -> ([ Obj ], RandStdGen)
updateObjCluster t acc rand objsWithDist  =
  case objsWithDist of
    [] -> (acc, rand)
    objWithDist :: tl ->
      let (updated, rand') = updateObj t tl rand objWithDist.obj
      in  updateObjCluster t (updated ++ acc) rand' tl 

updateObjs t world =
  let orderedObjClusters = Physics.ObjOrderer.orderedObjClusters world.objs
      (updated, rand') = foldl 
        (\objCluster -> \(acc, rand) -> updateObjCluster t acc rand objCluster) 
        ([], world.random) 
        orderedObjClusters 
  in  { world | objs <- updated
              , random <- rand' }

updateAnn world =
  let annFn = Physics.Annihilator.annihilateIfOutOfBounds world.ann
      annihilated = justs <| map annFn world.objs
  in  { world | objs <- annihilated } 

updateTimeQuant: Time -> World -> World
updateTimeQuant t = Physics.TrafficLights.update t . updateAnn . updateObjs t

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

manualTrafficLightsSwitch: World -> World
manualTrafficLightsSwitch = Physics.TrafficLights.enqueueManualSwitch 

startAutoTrafficLightsSwitch: TLAutoInt -> World -> World
startAutoTrafficLightsSwitch = Physics.TrafficLights.enqueueAutoSwitch
