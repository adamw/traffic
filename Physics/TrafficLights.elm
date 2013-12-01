module Physics.TrafficLights(update, manualSwitch, autoSwitch) where

import open Model

updateSteps newSteps world =
  let tlCtrl = world.tlCtrl
      newTlCtrl = { tlCtrl | steps <- newSteps }
  in  { world | tlCtrl <- newTlCtrl }

updateTlCtrl updateFn world = 
  let tlCtrl = world.tlCtrl
      tlCtrl' = updateFn tlCtrl
  in  { world | tlCtrl <- tlCtrl'  }

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

{--
We can switch TL controls if there are no steps or the only step left
is wait (which is the last step of automatic TL control)
--}
canSwitch steps =
  case steps of
    [] -> True
    [ WaitStep x ] -> True
    _ -> False

removeRenewSteps = updateTlCtrl <| \tlCtrl -> { tlCtrl | renewSteps <- Nothing }

manualSwitch: World -> World
manualSwitch = removeRenewSteps . \world ->
  if canSwitch world.tlCtrl.steps
  then let (redGroup, greenGroup) = assignGroups world
           steps = switchSteps world.tlCtrl redGroup greenGroup
       in  updateSteps (world.tlCtrl.steps ++ steps) world
  else world -- a switch is in progress

autoSwitch: TLAutoInt -> World -> World
autoSwitch int = updateTlCtrl <| \tlCtrl ->
  { tlCtrl | steps <- tlCtrl.steps ++ [ SetAutoInt int ] }

switchToState tlId newTlState world = 
  let updateTlFn = \tl -> if tl.tlId == tlId 
                          then { tl | state <- newTlState }
                          else tl
      updateFn = \obj -> case obj of
                           TrafficLight tl -> TrafficLight (updateTlFn tl)
                           _ -> obj
      newObjs = map updateFn world.objs
  in  { world | objs <- newObjs }

setAutoInt int world =
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
  in  updateTlCtrl (\tlCtrl -> { tlCtrl | renewSteps <- Just renewSteps }) world

{-- 
update the world according to the given step and time, and return an updated step
(if any), and remaining time
--}
updateWithStep: Time -> TLCtrlStep -> World -> (World, Maybe TLCtrlStep, Time)
updateWithStep t step world =
  case step of
    SwitchStep tlState tlId -> 
      (switchToState tlId tlState world, Nothing, t)
    WaitStep wt ->
      if wt > t then (world, Just (WaitStep (wt - t)), 0) else (world, Nothing, t - wt) 
    SetAutoInt int ->  
      (setAutoInt int world, Nothing, t)

update: Time -> World -> World 
update t world =
  case world.tlCtrl.steps of
    [] -> 
      case world.tlCtrl.renewSteps of
        Nothing -> world
        Just steps -> update t <| updateSteps steps world
    step :: other -> 
      let (updatedWorld, updatedStep, updatedT) = updateWithStep t step world
          newSteps = cons updatedStep other
          newWorld = updateSteps newSteps updatedWorld
          shouldContinue = isNothing updatedStep
      in  if shouldContinue 
          -- step disappeared, maybe we can execute more
          then update updatedT newWorld
          else newWorld

