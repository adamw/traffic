module Factories where

import open Model
import open UIModel

mainCanvas: SizeC
mainCanvas = { widthC = 1000, heightC = 500 }

initialViewportM: ViewportM
initialViewportM = { sizeM = { lengthM = 300, widthM = 150 },
                     centerM = { xM = 0, yM = 0 } }

createCar: ClusterId -> Float -> Car
createCar clusterId xOffset = { posM = { xM = xOffset, yM = 5 }, 
                                speedKph = 50,
                                sizeM = { lengthM = 4, widthM = 2 },
                                direction = degrees 0,
                                aMss = 4.3,
                                clusterId = clusterId  }

createCarCreator: ClusterId -> Float -> Float -> Float -> CarCreator
createCarCreator clusterId xOffset yOffset degs = { posM = { xM = xOffset, yM = yOffset },
                                                    direction = degrees degs,
                                                    clusterId = clusterId }

createTrafficLight: ClusterId -> Float -> Float -> Float -> TrafficLight
createTrafficLight clusterId xOffset yOffset degs = { posM = { xM = xOffset, yM = yOffset },
                                                      direction = degrees degs,
                                                      state = RedTrafficLight,
                                                      updateFn = noOpTLUpdateFn,
                                                      switchTimings = {
                                                        yellowAfterRed = 1.0 * oneSecond, 
                                                        yellowAfterGreen = 3.0 * oneSecond
                                                      },
                                                      clusterId = clusterId  
                                                    }                         

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

initialUIWorld: UIWorld
initialUIWorld = 
  let world = { objs = [ CarObj (createCar 1 -150), 
                         CarObj (createCar 1 -100), 
                         CarObj (createCar 1 -50), 
                         TrafficLightObj (createTrafficLight 2 -10  -5 0), -- L->R, bottom lane
                         TrafficLightObj (createTrafficLight 1 -10   5 0), -- L->R, top lane
                         TrafficLightObj (createTrafficLight 3 0    15 270), -- T->B                          
                         TrafficLightObj (createTrafficLight 4 10  -15 90), -- B->T
                         CarCreatorObj (createCarCreator 2 -160 -5 0),
                         CarCreatorObj (createCarCreator 1 -160  5 0),
                         CarCreatorObj (createCarCreator 3 0   80 270),
                         CarCreatorObj (createCarCreator 4 10 -80 90) 
                        ],
                ann = { minX = -200, maxX = 200, 
                        minY = -100, maxY = 100 } 
              }
  in  { viewport = initialWorldViewport,
        world = world, 
        info = "X",
        timeMultiplier = 1.0 }