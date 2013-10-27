module SimulationSpeed(speedOfSimulationUp, speedOfSimulationDown, adjustTime) where

import open WorldModel

possibleSpeeds = [ 0.5, 1.0, 1.5, 2.0, 4.0, 8.0, 16.0, 32.0 ]

speedOfSimulationUp: World -> World
speedOfSimulationUp world = changeSpeedIdx world (\i -> i + 1)

speedOfSimulationDown: World -> World
speedOfSimulationDown world = changeSpeedIdx world (\i -> i - 1)

adjustTime: World -> Float -> Float
adjustTime world t = t * world.timeMultiplier

changeSpeedIdx world changeIdxFn =
  let currentMultiplier = world.timeMultiplier
      currentIdx = indexOf currentMultiplier possibleSpeeds 0
      newIdx = changeIdxFn currentIdx
      clampedIdx = clamp 0 ((length possibleSpeeds) - 1) newIdx
      newMultiplier = atIndex clampedIdx possibleSpeeds
  in  { world | timeMultiplier <- newMultiplier }

indexOf el list idxCandidate =
  case list of
    [] -> -1
    hd :: tl -> if (el == hd) then idxCandidate else indexOf el tl (idxCandidate + 1)

atIndex idx list = if (idx == 0) then head list else atIndex (idx-1) (tail list)