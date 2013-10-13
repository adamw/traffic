module Factories where

import open Model
import open WorldModel

mainCanvas: SizeC
mainCanvas = { widthC = 1000, heightC = 500 }

initialViewportM: ViewportM
initialViewportM = { sizeM = { lengthM = 300, widthM = 150 },
                     centerM = { xM = 0, yM = 0 } }

createCar: Float -> Car
createCar xOffset = { posM = { xM = xOffset, yM = 0 }, 
                       speedKph = 50,
                       sizeM = { lengthM = 4, widthM = 2 },
                       direction = degrees 0,
                       aMss = 4.3 }

initialTrafficLight: ViewportM -> TrafficLight
initialTrafficLight viewportM = { posM = { xM = 0, yM = 0 },
                                  direction = degrees 0,
                                  state = RedTrafficLight,
                                  updateFn = noOpTLUpdateFn ,
                                  switchTimings = {
                                    yellowAfterRed = 1.0 * oneSecond, 
                                    yellowAfterGreen = 3.0 * oneSecond
                                  } 
                                }                         

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

initialWorld: World
initialWorld = { viewport = initialWorldViewport, 
                 objs = [ CarObj (createCar -150), 
                          CarObj (createCar -100), 
                          CarObj (createCar -50), 
                          TrafficLightObj (initialTrafficLight initialViewportM) ], 
                 info = "X" }