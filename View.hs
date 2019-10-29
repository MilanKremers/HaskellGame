-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure GameState{ship=s} = translate (getPos1 s) (getPos2 s) (color green (Circle 50))

getPos1 :: Player -> Float
getPos1 Player{ posPlayer = (p1, p2) } = p1 

getPos2 :: Player -> Float
getPos2 Player{ posPlayer = (p1, p2) } = p2