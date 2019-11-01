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
  | otherwise = return $ collisionDetection GameState{enemies = stepEnemies e, bullets = stepBullets b, ship = stepPlayer k s, isPaused = p, keys = k}

-- | functions handling the movement of the player
stepPlayer :: S.Set Key -> Player -> Player
stepPlayer k s | S.member (SpecialKey KeyUp)    k = s{posPX = posPX s, posPY = posPY s + 2}
               | S.member (SpecialKey KeyDown)  k = s{posPX = posPX s, posPY = posPY s - 2}
               | S.member (SpecialKey KeyLeft)  k = s{posPX = posPX s - 1, posPY = posPY s}
               | S.member (SpecialKey KeyRight) k = s{posPX = posPX s + 1, posPY = posPY s}
               | otherwise                        = s

-- | Functions handling the movement of bullets
stepBullets :: [Bullet] -> [Bullet]
stepBullets []     = []
stepBullets [b]    = [moveBullet b]
stepBullets (b:bs) = moveBullet b : stepBullets bs

moveBullet :: Bullet -> Bullet
moveBullet b | direction b == L = b{ posBX = posBX b - 5 }
             | otherwise        = b{ posBX = posBX b + 5 }

-- | Function handling spawning and moving enemies
stepEnemies :: [Enemy] -> [Enemy]
stepEnemies []     = []
stepEnemies [e]    = [e{posEX = posEX e - 0.5}]
stepEnemies (e:es) = e{posEX = posEX e - 0.5} : stepEnemies es

-- | checking for collisions
collisionDetection :: GameState -> GameState
collisionDetection gstate@GameState{enemies = e, bullets  = b, ship = s} = gstate{enemies = enemyCollision e b}

enemyCollision :: [Enemy] -> [Bullet] -> [Enemy]
enemyCollision [] _      = []
enemyCollision [e] bs    | elem True (map (checkCollision (posEX e) (posEY e)) bs) = []
                         | otherwise                                               = [e]
enemyCollision (e:es) bs | elem True (map (checkCollision (posEX e) (posEY e)) bs) = enemyCollision es bs
                         | otherwise                                               = e : enemyCollision es bs
                    
checkCollision :: Float -> Float -> Bullet -> Bool
checkCollision x y Bullet{posBX = bx, posBY = by} | bx < x + 15 && bx > x - 15 && by < y + 15 && by > y - 15 = True
                                                  | otherwise                                                = False

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (Char 'p') Down _ _) gstate | (isPaused gstate) == Play = return $ gstate{isPaused = Pause}
                                            | otherwise                 = return $ gstate{isPaused = Play}
input (EventKey (Char 'f') Down _ _) gstate = return $ gstate{bullets = (Bullet{ posBX = (posPX $ ship gstate), posBY = (posPY $ ship gstate), direction = R } : (bullets gstate))}
input (EventKey k Down _ _) gstate          = return $ gstate {keys = S.insert k (keys gstate)}
input (EventKey k Up _ _) gstate            = return $ gstate {keys = S.delete k (keys gstate)}
input _ gstate                              = return $ gstate


