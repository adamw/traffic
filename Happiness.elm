module Happiness(update, average, fraction, initial) where

import open Model

minH = -100
maxH = 50
borderH = -50
updateEvery = oneSecond

initial: Happiness
initial = { raw = 25, val = 25, nextUpdate = 0 }

update: Time -> World -> World
update t world = 
  let mapFn o = case o of
        Car c -> Car (updateUntilNextUpdateInFuture . addToCarHappinessNextUpdate (-t) <| c)
        _     -> o
      objs' = map mapFn world.objs
  in  { world | objs <- objs' }

updateUntilNextUpdateInFuture: Car -> Car
updateUntilNextUpdateInFuture car =
  if car.happiness.nextUpdate < 0
  then let car' = addToCarHappinessNextUpdate updateEvery car
           car'' = updateHappiness car'
        in updateUntilNextUpdateInFuture car''
  else car

addToCarHappinessNextUpdate delta car =
  let happiness = car.happiness
      nextUpdate' = happiness.nextUpdate + delta
      happiness' = { happiness | nextUpdate <- nextUpdate' }
  in { car | happiness <- happiness' }

updateHappiness car =
  let v = car.speedKph
      h = car.happiness
      rawDelta = if | v == 0    -> -3
                    | v < 10    ->  1
                    | v < 25    ->  2
                    | otherwise ->  4
      -- if the happiness is increasing, then even if we below -100 (raw),
      -- then we want to go start from it
      raw' = if rawDelta > 0
             then h.val + rawDelta
             else clamp minH maxH <| h.raw + rawDelta
      -- val stays at -100, and then jumps to -200 as the driver looses hope
      val' = if | raw' > borderH    -> raw'
                | raw' == minH      -> raw'
                | h.val < borderH   -> raw' -- lost hope before
                | otherwise         -> borderH
      h' = { h | raw <- raw', val <- val' }
  in  { car | happiness <- h' }

average: World -> Int
average world =
  let 
    sumH o (s, c) = case o of 
      Car car -> (s + car.happiness.val, c + 1)
      _ -> (s, c)
    (totalS, totalC) = foldl sumH (0, 0) world.objs
  in round <| (toFloat totalS) / (toFloat totalC)

fraction: Car -> Float
fraction car =
  let span = maxH - minH
      offset = car.happiness.val - minH
  in  (toFloat offset) / (toFloat span)