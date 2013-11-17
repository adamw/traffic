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

createTrafficLight: ClusterId -> TLId -> TLState -> Float -> Float -> Float -> TrafficLight
createTrafficLight clusterId tlId tlState xOffset yOffset degs = 
  { tlId = tlId,
    posM = { xM = xOffset, yM = yOffset },
    direction = degrees degs,
    state = tlState,
    clusterId = clusterId  
  }                         

initialWorldViewport: WorldViewport
initialWorldViewport = { viewportM = initialViewportM, canvas = mainCanvas }

initialUIWorld: UIWorld
initialUIWorld = 
  let worldObjs = [ Car (createCar 1 -150), 
                    Car (createCar 1 -100), 
                    Car (createCar 1 -50), 
                    TrafficLight (createTrafficLight 2 1 RedTrafficLight   -10  -5 0), -- L->R, bottom lane
                    TrafficLight (createTrafficLight 1 2 RedTrafficLight   -10   5 0), -- L->R, top lane
                    TrafficLight (createTrafficLight 3 3 GreenTrafficLight 0    15 270), -- T->B                          
                    TrafficLight (createTrafficLight 4 4 GreenTrafficLight 10  -15 90), -- B->T
                    CarCreator (createCarCreator 2 -160 -5 0),
                    CarCreator (createCarCreator 1 -160  5 0),
                    CarCreator (createCarCreator 3 0   80 270),
                    CarCreator (createCarCreator 4 10 -80 90) 
                  ]
      world = { objs = worldObjs,
                ann = { minX = -200, maxX = 200, 
                        minY = -100, maxY = 100 },
                tlCtrl = {
                  groupA = [ 1, 2 ],
                  groupB = [ 3, 4 ],
                  yellowAfterRed = 1.0 * oneSecond, 
                  yellowAfterGreen = 3.0 * oneSecond,
                  betweenRed = 1.0 * oneSecond,
                  steps = []
                } 
              }
  in  { viewport = initialWorldViewport,
        world = world, 
        info = "X",
        timeMultiplier = 1.0 }