module Physics(updateObjs, toggleTrafficLight) where 

import open Model
import Physics.Car
import Physics.TrafficLight
import Physics.CarCreator

updateObj: Time -> [ Obj ] -> Obj -> [ Obj ]
updateObj t allObjs obj =
  case obj of
    CarObj car -> [ CarObj (Physics.Car.drive t allObjs car) ]
    TrafficLightObj tl -> [ TrafficLightObj (Physics.TrafficLight.update t tl) ]
    CarCreatorObj cc  -> cons (Physics.CarCreator.createIfVacant cc allObjs) [ obj ]
    _ -> [ obj ]

updateObjsTimeQuant: Time -> [ Obj ] -> [ Obj ]
updateObjsTimeQuant t objs = foldr ((++) . updateObj t objs) [] objs

timeQuantMs = 100

{--
We assume that decisions can be changed every 100ms. Hence if the time span to cover
is longer, we chunk it into smaller pieces.
--}
updateObjs: Time -> [ Obj ] -> [ Obj ]
updateObjs t objs = 
  if (t > timeQuantMs) 
    then updateObjs (t-timeQuantMs) (updateObjsTimeQuant timeQuantMs objs) 
    else updateObjsTimeQuant t objs

toggleIfTrafficLight obj =
  case obj of
    TrafficLightObj tl -> TrafficLightObj (Physics.TrafficLight.toggle tl)
    _ -> obj

toggleTrafficLight: [ Obj ] -> [ Obj ]
toggleTrafficLight objs = map (toggleIfTrafficLight) objs