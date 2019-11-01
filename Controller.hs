{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Set as S


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate@GameState{enemies = e, bullets  = b, ship = s, isPaused = p, keys = k}
          | p == Pause = return $ gstate  
          | otherwise = return $ GameState{enemies = e, bullets = stepBullets b, ship = movePlayer k s, isPaused = p, keys = k}

-- | functions handling the movement of the player

movePlayer :: S.Set Key -> Player -> Player
movePlayer k s | S.member (SpecialKey KeyUp)    k = setPlayerPosFromState s (0, 20)
               | S.member (SpecialKey KeyDown)  k = setPlayerPosFromState s (0, -20)
               | S.member (SpecialKey KeyLeft)  k = setPlayerPosFromState s (-10, 0)
               | S.member (SpecialKey KeyRight) k = setPlayerPosFromState s (10, 0)
               | otherwise                        = s

getPosX :: Player -> Float
getPosX Player{ posPlayer = (p1, p2) } = p1 

getPosY :: Player -> Float
getPosY Player{ posPlayer = (p1, p2) } = p2

setPlayerPosFromState :: Player -> (Float, Float) -> Player
setPlayerPosFromState s newPos@(p1, p2) | p1 == 0 && p2 > 0 && getPosY s < 540  = newShip
                                        | p1 == 0 && p2 < 0 && getPosY s > -500 = newShip
                                        | p2 == 0 && p1 > 0 && getPosX s < 940  = newShip
                                        | p2 == 0 && p1 < 0 && getPosX s > -940 = newShip
                                        | otherwise                             = s
  where newShip = s{posPlayer = addPos (posPlayer s) newPos}

addPos :: (Float, Float) -> (Float, Float) -> (Float, Float)
addPos (p1, p2) (p3, p4) = (p1 + p3, p2 + p4)

-- | Functions handling the movement of bullets
stepBullets :: [Bullet] -> [Bullet]
stepBullets [] = []
stepBullets [b] = [moveBullet b]
stepBullets (b:bs) = moveBullet b : stepBullets bs

moveBullet :: Bullet -> Bullet
moveBullet Bullet{ posBullet = (p1, p2) , direction = r } | r == L    = Bullet{ posBullet = ((p1 - 50), p2) , direction = r }
                                                          | otherwise = Bullet{ posBullet = ((p1 + 50), p2) , direction = r }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (Char 'p') Down _ _) gstate | (isPaused gstate) == Play = return $ gstate{isPaused = Pause}
                                            | otherwise                 = return $ gstate{isPaused = Play}
input (EventKey (Char 'f') Down _ _) gstate = return $ gstate{bullets = (Bullet{ posBullet = (posPlayer $ ship gstate), direction = R } : (bullets gstate))}
input (EventKey k Down _ _) gstate = return $ gstate {keys = S.insert k (keys gstate)}
input (EventKey k Up _ _) gstate = return $ gstate {keys = S.delete k (keys gstate)}
input _ gstate = return $ gstate


