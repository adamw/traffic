module Physics.TrafficLights(update, enqueueManualSwitch, enqueueAutoSwitch) where

import open Model

updateTlCtrl updateFn world = 
  let tlCtrl = world.tlCtrl
      tlCtrl' = updateFn tlCtrl
  in  { world | tlCtrl <- tlCtrl'  }

updateSteps newSteps = updateTlCtrl (\tlCtrl -> { tlCtrl | steps <- newSteps })

assignGroups world =
  let groupLR = world.tlCtrl.groupLR
      groupTD = world.tlCtrl.groupTD
      groupLRTl = head groupLR
      isRedFromLR = \obj -> case obj of
        TrafficLight tl -> tl.tlId == groupLRTl && tl.state == RedTrafficLight
        _ -> False
      groupLRRed = any isRedFromLR world.objs
  in  if (groupLRRed) then (groupLR, groupTD) else (groupTD, groupLR)

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

enqueueManualSwitch: World -> World
enqueueManualSwitch = updateTlCtrl <| \tlCtrl ->
  { tlCtrl | steps <- tlCtrl.steps ++ [ ManualSwitch ] }

enqueueAutoSwitch: TLAutoInt -> World -> World
enqueueAutoSwitch int = updateTlCtrl <| \tlCtrl ->
  { tlCtrl | steps <- tlCtrl.steps ++ [ SetAutoSwitch int ] }

switchToState tlId newTlState world = 
  let updateTlFn = \tl -> if tl.tlId == tlId 
                          then { tl | state <- newTlState }
                          else tl
      updateFn = \obj -> case obj of
                           TrafficLight tl -> TrafficLight (updateTlFn tl)
                           _ -> obj
      newObjs = map updateFn world.objs
  in  { world | objs <- newObjs }

setAutoSwitch int world =
  let (redGroup, greenGroup) = assignGroups world
      isLRGreen = greenGroup == world.tlCtrl.groupLR
      (intAfterRedGroup, intAfterGreenGroup) = if isLRGreen 
        then (int.td, int.lr) 
        else (int.lr, int.td) 
      renewSteps = concat [
        switchSteps world.tlCtrl redGroup greenGroup,
        [ WaitStep (toFloat intAfterRedGroup*oneSecond) ],
        switchSteps world.tlCtrl greenGroup redGroup,
        [ WaitStep (toFloat intAfterGreenGroup*oneSecond) ]
      ]
  in  updateTlCtrl (\tlCtrl -> { tlCtrl | autoSwitch <- Just (int, renewSteps) }) world

removeRenewSteps = updateTlCtrl <| \tlCtrl -> { tlCtrl | autoSwitch <- Nothing }

manualSwitchSteps world = 
  if length world.tlCtrl.steps <= 1 -- the current step is on the list still
  then let (redGroup, greenGroup) = assignGroups world
       in   switchSteps world.tlCtrl redGroup greenGroup
  else [] -- more steps enqueued later

{-- 
update the world according to the given step and time, and return an updated step
(if any), and remaining time
--}
updateWithStep: Time -> TLCtrlStep -> World -> (World, Maybe TLCtrlStep, [ TLCtrlStep ], Time)
updateWithStep t step world =
  case step of
    SwitchStep tlState tlId -> 
      (switchToState tlId tlState world, Nothing, [], t)
    WaitStep wt ->
      if wt > t then (world, Just (WaitStep (wt - t)), [], 0) else (world, Nothing, [], t - wt) 
    SetAutoSwitch int ->  
      (setAutoSwitch int world, Nothing, [], t)
    ManualSwitch ->  
      (removeRenewSteps world, Nothing, manualSwitchSteps world, t)

update: Time -> World -> World 
update t world =
  case world.tlCtrl.steps of
    [] -> 
      case world.tlCtrl.autoSwitch of
        Nothing -> world
        Just (_, steps) -> update t <| updateSteps steps world
    step :: other -> 
      let (updatedWorld, updatedStep, appendSteps, updatedT) = updateWithStep t step world
          newSteps = (cons updatedStep other) ++ appendSteps
          newWorld = updateSteps newSteps updatedWorld
          shouldContinue = isNothing updatedStep
      in  if shouldContinue 
          -- step disappeared, maybe we can execute more
          then update updatedT newWorld
          else newWorld

