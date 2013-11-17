module Physics.TrafficLights(update, startSwitch) where

import open Model

updateSteps newSteps world =
  let tlCtrl = world.tlCtrl
      newTlCtrl = { tlCtrl | steps <- newSteps }
  in  { world | tlCtrl <- newTlCtrl }

assignGroups world =
  let groupA = world.tlCtrl.groupA
      groupB = world.tlCtrl.groupB
      groupATl = head groupA
      isRedFromA = \obj -> case obj of
        TrafficLight tl -> tl.tlId == groupATl && tl.state == RedTrafficLight
        _ -> False
      groupARed = any isRedFromA world.objs
  in  if (groupARed) then (groupA, groupB) else (groupB, groupA)

switchSteps tlCtrl redGroup greenGroup =
  concat [
    map (SwitchStep YellowTrafficLight) greenGroup,
    [ WaitStep tlCtrl.yellowAfterGreen ],
    map (SwitchStep RedTrafficLight) greenGroup,
    [ WaitStep tlCtrl.betweenRed ],
    map (SwitchStep YellowTrafficLight) redGroup,
    [ WaitStep tlCtrl.yellowAfterRed ],
    map (SwitchStep GreenTrafficLight) redGroup
  ]

startSwitch: World -> World
startSwitch world =
  if isEmpty world.tlCtrl.steps
  then let (redGroup, greenGroup) = assignGroups world
           steps = switchSteps world.tlCtrl redGroup greenGroup
       in  updateSteps steps world
  else world -- a switch is alread in progress

switchToState tlId newTlState world = 
  let updateTlFn = \tl -> if tl.tlId == tlId 
                          then { tl | state <- newTlState }
                          else tl
      updateFn = \obj -> case obj of
                           TrafficLight tl -> TrafficLight (updateTlFn tl)
                           _ -> obj
      newObjs = map updateFn world.objs
  in  { world | objs <- newObjs }

{-- 
update the world according to the given step and time, and return an updated step
(if any), and remiaining time
--}
updateWithStep: Time -> TLCtrlStep -> World -> (World, Maybe TLCtrlStep, Time)
updateWithStep t step world =
  case step of
    SwitchStep tlState tlId -> 
      (switchToState tlId tlState world, Nothing, t)
    WaitStep wt ->
      if wt > t then (world, Just (WaitStep (wt - t)), 0) else (world, Nothing, t - wt)   

update: Time -> World -> World 
update t world =
  case world.tlCtrl.steps of
    [] -> world
    step :: other -> 
      let (updatedWorld, updatedStep, updatedT) = updateWithStep t step world
          newSteps = cons updatedStep other
          newWorld = updateSteps newSteps updatedWorld
          shouldContinue = isNothing updatedStep
      in  if shouldContinue 
          -- step disappeared, maybe we can execute more
          then update updatedT newWorld
          else newWorld

