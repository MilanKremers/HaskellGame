-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . view' . viewPure

view' :: [Picture] -> Picture
view' xs = Pictures xs

viewPure :: GameState -> [Picture]
viewPure g = drawShip g : drawBullets g ++ drawEnemies g ++ []

drawShip :: GameState -> Picture
drawShip GameState{ship = s} = translate (posPX s) (posPY s) (color green (Circle 50)) 

drawBullets :: GameState -> [Picture]
drawBullets GameState{bullets = bullets} = drawBullets' bullets

drawBullets' :: [Bullet] -> [Picture]
drawBullets' []     = []
drawBullets' [b]    = translate (posBX b) (posBY b) (color red (Circle 5)) : []
drawBullets' (b:bs) = translate (posBX b) (posBY b) (color red (Circle 5)) : drawBullets' bs 

drawEnemies :: GameState -> [Picture]
drawEnemies GameState{enemies = enemies} = drawEnemies' enemies

drawEnemies' :: [Enemy] -> [Picture]
drawEnemies' []     = []
drawEnemies' [e]    = translate (posEX e) (posEY e) (color yellow (Circle 15)) : []
drawEnemies' (e:es) = translate (posEX e) (posEY e) (color yellow (Circle 15)) : drawEnemies' es

