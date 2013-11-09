module SimulationSpeed(speedOfSimulationUp, speedOfSimulationDown, adjustTime) where

import open UIModel

possibleSpeeds = [ 0.5, 1.0, 1.5, 2.0, 4.0, 8.0, 16.0, 32.0 ]

speedOfSimulationUp: UIWorld -> UIWorld
speedOfSimulationUp uiworld = changeSpeedIdx uiworld (\i -> i + 1)

speedOfSimulationDown: UIWorld -> UIWorld
speedOfSimulationDown uiworld = changeSpeedIdx uiworld (\i -> i - 1)

adjustTime: UIWorld -> Float -> Float
adjustTime uiworld t = t * uiworld.timeMultiplier

changeSpeedIdx uiworld changeIdxFn =
  let currentMultiplier = uiworld.timeMultiplier
      currentIdx = indexOf currentMultiplier possibleSpeeds 0
      newIdx = changeIdxFn currentIdx
      clampedIdx = clamp 0 ((length possibleSpeeds) - 1) newIdx
      newMultiplier = atIndex clampedIdx possibleSpeeds
  in  { uiworld | timeMultiplier <- newMultiplier }

indexOf el list idxCandidate =
  case list of
    [] -> -1
    hd :: tl -> if (el == hd) then idxCandidate else indexOf el tl (idxCandidate + 1)

atIndex idx list = if (idx == 0) then head list else atIndex (idx-1) (tail list)