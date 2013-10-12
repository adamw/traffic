module Traffic where

import Factories
import UI

main = UI.layout <~ foldp UI.worldStep Factories.initialWorld UI.inputSignal
