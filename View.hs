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
viewPure g = drawShip g : drawBullets g ++ []

drawShip :: GameState -> Picture
drawShip GameState{ship = s} = translate (getPos1 s) (getPos2 s) (color green (Circle 50)) 

drawBullets :: GameState -> [Picture]
drawBullets GameState{bullets = bullets} = drawBullets' bullets

drawBullets' :: [Bullet] -> [Picture]
drawBullets' []     = []
drawBullets' [b]    = translate (getPos1B b) (getPos2B b) (color red (Circle 5)) : []
drawBullets' (b:bs) = translate (getPos1B b) (getPos2B b) (color red (Circle 5)) : drawBullets' bs 

getPos1 :: Player -> Float
getPos1 Player{ posPlayer = (p1, p2) } = p1 

getPos2 :: Player -> Float
getPos2 Player{ posPlayer = (p1, p2) } = p2

getPos1B :: Bullet -> Float
getPos1B Bullet{ posBullet = (p1, p2) } = p1 

getPos2B :: Bullet -> Float
getPos2B Bullet{ posBullet = (p1, p2) } = p2