module Planet where

import           GHC.Word (Word8)
import           SDL

-- Structure used for storing data about planets
data Planet = Planet
    { mass     :: Float
    , position :: V2 Float
    , velocity :: V2 Float
    , color    :: V4 Word8
    }
    deriving Eq

sMag :: V2 Float -> Float
sMag v = sum $ (**2) <$> v
mag = sqrt . sMag

update :: Float -> Float -> [Planet] -> Planet -> Planet
update dt gc othPlanets planet = do
    let acceleration = sum $ map (
            \op -> do
                let sdst = sMag (position op - position planet)
                let val = (gc * mass op) / sdst
                (position op - position planet) * pure (val / sqrt sdst)
                                 ) $ filter (/=planet) othPlanets

    let newVelocity = velocity planet + acceleration * pure dt
    let newPosition = position planet + (velocity planet * pure dt)

    planet { position = newPosition
           , velocity = newVelocity
           }

