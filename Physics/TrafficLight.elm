module Physics.TrafficLight(update, toggle) where

import open Model

switchToUpdateFn: TLState -> Time -> TLUpdateFn
switchToUpdateFn targetState targetAfterTime = TLUpdateFn { fn = \(currentState, t) ->
  let newAfterTime = targetAfterTime - t
  in  if newAfterTime > 0 
      then (currentState, switchToUpdateFn targetState newAfterTime)
      else (targetState, noOpTLUpdateFn) 
  }

switchTo: TLState -> Float -> TrafficLight -> TrafficLight
switchTo targetState yellowDuration tl =
  { tl | state <- YellowTrafficLight
       , updateFn <- switchToUpdateFn targetState yellowDuration }

update: Time -> TrafficLight -> TrafficLight
update t tl = 
  let oldState = tl.state
      updateFn = case tl.updateFn of TLUpdateFn tlUpdateFn -> tlUpdateFn.fn
      (newState, newUpdateFn) = updateFn (oldState, t)
  in  { tl | state <- newState
           , updateFn <- newUpdateFn }  

toggle: TrafficLight -> TrafficLight
toggle tl = case tl.state of
  RedTrafficLight -> switchTo GreenTrafficLight tl.switchTimings.yellowAfterRed tl
  YellowTrafficLight -> tl -- switch is in progress, doing nothing
  GreenTrafficLight -> switchTo RedTrafficLight tl.switchTimings.yellowAfterGreen tl
  