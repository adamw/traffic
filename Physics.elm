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

updateObjsTimeQuant: Time -> Annihilator -> [ Obj ] -> [ Obj ]
updateObjsTimeQuant t ann objs = 
  let orderedObjClusters = Physics.ObjOrderer.orderedObjClusters objs
      updatedClusters = map (updateObjCluster t []) orderedObjClusters
      updated = concat updatedClusters
  in  justs <| map (Physics.Annihilator.annihilateIfOutOfBounds ann) updated 

timeQuantMs = 100

{--
We assume that decisions can be changed every 100ms. Hence if the time span to cover
is longer, we chunk it into smaller pieces.
--}
updateObjs: Time -> Annihilator -> [ Obj ] -> [ Obj ]
updateObjs t ann objs = 
  if (t > timeQuantMs) 
    then updateObjs (t-timeQuantMs) ann (updateObjsTimeQuant timeQuantMs ann objs) 
    else updateObjsTimeQuant t ann objs

toggleIfTrafficLight obj =
  case obj of
    TrafficLightObj tl -> TrafficLightObj (Physics.TrafficLight.toggle tl)
    _ -> obj

toggleTrafficLight: [ Obj ] -> [ Obj ]
toggleTrafficLight objs = map (toggleIfTrafficLight) objs