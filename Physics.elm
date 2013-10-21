module Physics(updateObjs, toggleTrafficLight) where 

import open Model
import Physics.Car
import Physics.TrafficLight
import Physics.CarCreator
import Physics.Annihilator

updateObj: Time -> [ Obj ] -> Obj -> [ Obj ]
updateObj t allObjs obj =
  case obj of
    CarObj car -> [ CarObj (Physics.Car.drive t allObjs car) ]
    TrafficLightObj tl -> [ TrafficLightObj (Physics.TrafficLight.update t tl) ]
    CarCreatorObj cc  -> cons (Physics.CarCreator.createIfVacant cc allObjs) [ obj ]
    _ -> [ obj ]

updateObjsTimeQuant: Time -> Annihilator -> [ Obj ] -> [ Obj ]
updateObjsTimeQuant t ann objs = 
  let updated = foldr ((++) . updateObj t objs) [] objs
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