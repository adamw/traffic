module Traffic where

import Factories
import UI

main = UI.layoutSignal ~ foldp UI.uiworldStep Factories.initialUIWorld UI.inputSignal
