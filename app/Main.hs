{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Foreign.C.Types (CInt)
import           Planet          (Planet (Planet), color, mass, position,
                                  update, velocity)
import           SDL

-- Planet size scaler
planetSize = 1
-- Gravity constant G
gravityC = 0.0001

toCInt :: Float -> CInt
toCInt = round

-- Main simulation loop
mainLoop :: Renderer -> Float -> [Planet] -> IO ()
mainLoop renderer dt planets = do
    events <- pollEvents

    let eventIsQuit ev = eventPayload ev == QuitEvent

    let eventIsKPressed kc ev =
            case eventPayload ev of
              KeyboardEvent kbEv ->
                  keyboardEventKeyMotion kbEv == Pressed &&
                      keysymKeycode (keyboardEventKeysym kbEv) == kc
              _ -> False

    let quitting = any (eventIsKPressed KeycodeQ) events || any eventIsQuit events

    let speedUp = any (eventIsKPressed KeycodeW) events
    let speedDown = any (eventIsKPressed KeycodeS) events

    -- update
    let planets' = map (Planet.update dt gravityC planets) planets

    let newDt
          | speedUp = dt * 1.1
          | speedDown = dt * 0.9
          | otherwise = dt

    -- render
    rendererDrawColor renderer $= V4 10 10 10 255
    clear renderer


    mapM_ (\planet -> do
        let rad = planetSize * (mass planet ** 0.3)
        rendererDrawColor renderer $= Planet.color planet
        fillRect renderer $ Just $
            Rectangle
                (P (toCInt <$> position planet - pure (rad / 2)))
                (pure $ toCInt rad))
            planets'

    present renderer

    -- loop again
    if quitting then return ()
                else mainLoop renderer newDt planets'

redC = V4 255 100 50 255
yellowC = V4 255 255 0 255
greenC = V4 0 255 0 255
blueC = V4 100 100 255 255
grayC = V4 150 150 150 255

-- Default state of planets at beginning of simulation
defaultPlanets =
    [ Planet    { Planet.mass = 100000
                , Planet.position = V2 500 500
                , Planet.velocity = V2 0 0
                , Planet.color = yellowC 
                }
    , Planet    { Planet.mass = 20
                , Planet.position = V2 600 500
                , Planet.velocity = V2 0 0.3
                , Planet.color = redC 
                }
    , Planet    { Planet.mass = 200
                , Planet.position = V2 800 500
                , Planet.velocity = V2 0 0.18
                , Planet.color = blueC 
                }
    , Planet    { Planet.mass = 50
                , Planet.position = V2 805 500
                , Planet.velocity = V2 0 0.25
                , Planet.color = grayC 
                }
    , Planet    { Planet.mass = 1000
                , Planet.position = V2 100 500
                , Planet.velocity = V2 0 (-0.16)
                , Planet.color = redC 
                }
    , Planet    { Planet.mass = 50
                , Planet.position = V2 90 500
                , Planet.velocity = V2 0 (-0.07) 
                , Planet.color = greenC
                }
    , Planet    { Planet.mass = 100
                , Planet.position = V2 120 500
                , Planet.velocity = V2 0 (-0.08) 
                , Planet.color = blueC
                }
    ]

main :: IO ()
main = do
    initialize [ InitVideo ]
    putStrLn "Initialized!"

    window <- createWindow "My gravity sim" $ defaultWindow { windowInitialSize = V2 1000 1000 }
    renderer <- createRenderer window (-1) (defaultRenderer {rendererType = AcceleratedRenderer})

    mainLoop renderer 0.05 defaultPlanets

    putStrLn "Quitting!"
    destroyRenderer renderer
    destroyWindow window
    quit
