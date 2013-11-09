module Physics(update, switchTrafficLights) where 

import open Model
import Physics.Car
import Physics.TrafficLights
import Physics.CarCreator
import Physics.Annihilator
import Physics.ObjOrderer

updateObj: Time -> [ ObjWithDist ] -> Obj -> [ Obj ]
updateObj t objsAheadWithDist obj =
  case obj of
    CarObj car -> [ CarObj (Physics.Car.drive t objsAheadWithDist car) ]
    CarCreatorObj cc -> cons (Physics.CarCreator.createIfVacant cc objsAheadWithDist) [ obj ]
    _ -> [ obj ]

updateObjCluster: Time -> [ Obj ] -> [ ObjWithDist ] -> [ Obj ]
updateObjCluster t acc objsWithDist  =
  case objsWithDist of
    [] -> acc
    objWithDist :: tl ->
      let updated = updateObj t tl objWithDist.obj
      in  updateObjCluster t (updated ++ acc) tl 

updateWorldObjs world newObjs = { world | objs <- newObjs }

updateTimeQuant: Time -> World -> World
updateTimeQuant t world = 
  let orderedObjClusters = Physics.ObjOrderer.orderedObjClusters world.objs
      updatedClusters = map (updateObjCluster t []) orderedObjClusters
      updated = concat updatedClusters
      annFn = Physics.Annihilator.annihilateIfOutOfBounds world.ann
      annihilated = justs <| map annFn updated
      updatedWorld = updateWorldObjs world annihilated
  in  Physics.TrafficLights.update t updatedWorld

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

