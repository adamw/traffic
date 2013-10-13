module Physics(updateObjs, toggleTrafficLight) where 

import open Model
import Physics.Car
import Physics.TrafficLight

updateObj: Time -> [ Obj ] -> Obj -> Obj
updateObj t allObjs obj =
  case obj of
    CarObj car -> CarObj (Physics.Car.drive t allObjs car)
    TrafficLightObj tl -> TrafficLightObj (Physics.TrafficLight.update t tl)

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

toggleIfTrafficLight obj =
  case obj of
    TrafficLightObj tl -> TrafficLightObj (Physics.TrafficLight.toggle tl)
    _ -> obj

toggleTrafficLight: [ Obj ] -> [ Obj ]
toggleTrafficLight objs = map (toggleIfTrafficLight) objs