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
viewPure g = drawGameOver g : drawShip g : drawBullets g ++ drawEnemies g ++ drawExplosion g ++[]

drawGameOver :: GameState -> Picture
drawGameOver GameState{ isPaused = s } | s == GameOver = translate (-500) 0 (scale 0.5 0.5 (color red (text "Game Over, press r to restart")))
                                       | otherwise     = text ""

drawShip :: GameState -> Picture
drawShip GameState{ship = s} = translate (posPX s) (posPY s) (color green (Circle 50)) 

drawBullets :: GameState -> [Picture]
drawBullets GameState{bullets = bullets} = drawBullets' bullets

drawBullets' :: [Bullet] -> [Picture]
drawBullets' []     = []
drawBullets' [b]    = translate (posBX b) (posBY b) (color red (Circle 5)) : []
drawBullets' (b:bs) = translate (posBX b) (posBY b) (color red (Circle 5)) : drawBullets' bs 

drawExplosion :: GameState -> [Picture]
drawExplosion GameState{enemies = enemies} = drawExplosion' enemies

drawExplosion' :: [Enemy] -> [Picture]
drawExplosion' []     = []
drawExplosion' [x]    | (animation x) == True = translate (posEX x) (posEY x) (color orange (ThickCircle (50 + (animation2 x * 2)) 10)) : []
                      | otherwise = []
drawExplosion' (x:xs) | (animation x) == True = translate (posEX x) (posEY x) (color orange (ThickCircle (50 + (animation2 x * 2)) 10)) : drawExplosion' xs
                      | otherwise = drawExplosion' xs
                
 
drawEnemies :: GameState -> [Picture]
drawEnemies GameState{enemies = enemies} = drawEnemies' enemies

drawEnemies' :: [Enemy] -> [Picture]
drawEnemies' []     = []
drawEnemies' [e]    | animation e == False = translate (posEX e) (posEY e) (color yellow (Circle 15)) : []
                    | otherwise = []
drawEnemies' (e:es) | animation e == False = translate (posEX e) (posEY e) (color yellow (Circle 15)) : drawEnemies' es
                    | otherwise = drawEnemies' es


