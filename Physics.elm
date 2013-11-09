module Physics(updateObjs, toggleTrafficLight) where 

import open Model
import Physics.Car
import Physics.TrafficLight
import Physics.CarCreator
import Physics.Annihilator
import Physics.ObjOrderer

updateObj: Time -> [ ObjWithDist ] -> Obj -> [ Obj ]
updateObj t objsAheadWithDist obj =
  case obj of
    CarObj car -> [ CarObj (Physics.Car.drive t objsAheadWithDist car) ]
    TrafficLightObj tl -> [ TrafficLightObj (Physics.TrafficLight.update t tl) ]
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

updateObjsTimeQuant: Time -> World -> World
updateObjsTimeQuant t world = 
  let orderedObjClusters = Physics.ObjOrderer.orderedObjClusters world.objs
      updatedClusters = map (updateObjCluster t []) orderedObjClusters
      updated = concat updatedClusters
      annFn = Physics.Annihilator.annihilateIfOutOfBounds world.ann
      annihilated = justs <| map annFn updated
  in  updateWorldObjs world annihilated

timeQuantMs = 100

{--
We assume that decisions can be changed every 100ms. Hence if the time span to cover
is longer, we chunk it into smaller pieces.
--}
updateObjs: Time -> World -> World
updateObjs t world = 
  if (t > timeQuantMs) 
    then updateObjs (t-timeQuantMs) (updateObjsTimeQuant timeQuantMs world) 
    else updateObjsTimeQuant t world

toggleIfTrafficLight obj =
  case obj of
    TrafficLightObj tl -> TrafficLightObj (Physics.TrafficLight.toggle tl)
    _ -> obj

toggleTrafficLight: World -> World
toggleTrafficLight world = updateWorldObjs world <| map (toggleIfTrafficLight) world.objs