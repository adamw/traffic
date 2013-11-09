module Traffic where

import Factories
import UI

main = UI.layout <~ foldp UI.uiworldStep Factories.initialUIWorld UI.inputSignal
